-- |
-- Module      :  HashedExpression.Codegen.CSimple
-- Copyright   :  (c) OCA 2020
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  anandc@mcmaster.ca
-- Stability   :  provisional
-- Portability :  unportable
--
-- This module provides a backend for c code generation (the interface being provided by 'HashedExpression.Codegen') that provides no
-- parallelization (i.e no threading or SIMD)
module HashedExpression.Codegen.CSimple where

import Control.Monad (forM_, when)
import Data.Array (indices, (!))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (find, foldl', intercalate, partition, sortOn, tails)
import Data.List.HT (viewR)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import HashedExpression.Codegen
import HashedExpression.Embed (fftUtils)
import HashedExpression.Internal (topologicalSortManyRoots, unwrap)
import HashedExpression.Internal.Expression (ET (..), Expression, ExpressionMap, NumType, Op (..), Shape, exMap)
import HashedExpression.Internal.Node (retrieveElementType, retrieveNode, retrieveOp, retrieveShape)
import HashedExpression.Internal.Utils
import HashedExpression.Problem
import HashedExpression.Value
import System.FilePath
import Prelude hiding ((!!))

-------------------------------------------------------------------------------

data DataOutput = OutputText | OutputHDF5 deriving (Eq, Show)

-- | Generate simple C code
data CSimpleConfig = CSimpleConfig
  { output :: DataOutput
  }
  deriving (Eq, Show)

-- | Offset w.r.t "ptr"
type Address = Int

type NodeID = Int

-- | e.g: i, j, k
type Index = Text

data CSimpleCodegen = CSimpleCodegen
  { cExpressionMap :: ExpressionMap,
    cAddress :: NodeID -> Address,
    cMemSize :: Int,
    (!!) :: NodeID -> Index -> Text,
    imAt :: NodeID -> Index -> Text,
    reAt :: NodeID -> Index -> Text,
    config :: CSimpleConfig
  }

data CCode
  = Assign Text Text
  | Statement Text
  | Control Text [CCode]
  | Empty
  | Scoped [CCode]
  | Printf [Text]

fromCCode :: CCode -> Code
fromCCode c = case c of
  Assign lhs rhs -> [lhs <> " = " <> rhs <> ";"]
  Statement ss -> [ss <> ";"]
  Control control codes -> [control] ++ scoped (concatMap fromCCode codes)
  Empty -> []
  Scoped codes -> scoped (concatMap fromCCode codes)
  Printf [] -> []
  Printf (x : xs) -> ["printf(" <> T.intercalate ", " (("\"" <> x <> "\"") : xs) <> ");"]

-- | Helpers for code generation
scoped :: Code -> Code
scoped codes = ["{"] ++ indent 2 codes ++ ["}"]

-------------------------------------------------------------------------------

d2s :: Double -> Text
d2s val
  | val == ninf = "-INFINITY"
  | val == inf = "INFINITY"
  | otherwise = showT val

fun :: Text -> [Text] -> Text
fun f args = f <> "(" <> T.intercalate ", " args <> ")"

-- | Helper functions to generate codes
for :: Text -> Int -> [CCode] -> CCode
for iter bound codes =
  Scoped
    [ Statement $ "int " <> iter,
      Control
        ( "for ("
            <> (iter <> " = 0; ")
            <> (iter <> " < " <> showT bound <> "; ")
            <> (iter <> "++")
            <> ")"
        )
        codes
    ]

if_ :: Text -> [CCode] -> CCode
if_ condition = Control ("if (" <> condition <> ")")

elseif_ :: Text -> [CCode] -> CCode
elseif_ condition = Control ("else if (" <> condition <> ")")

else_ :: [CCode] -> CCode
else_ = Control "else"

forRange :: Text -> (Int, Int, Int) -> [CCode] -> CCode
forRange iter (start, end, step) codes =
  Scoped
    [ Statement $ "int " <> iter,
      Control
        ( "for ("
            <> (iter <> " = " <> showT start <> ";")
            <> (iter <> " <= " <> showT end <> "; ")
            <> (iter <> " += " <> showT step)
            <> ")"
        )
        codes
    ]

initCodegen :: CSimpleConfig -> ExpressionMap -> [NodeID] -> CSimpleCodegen
initCodegen config mp consecutiveIDs =
  CSimpleCodegen
    { cExpressionMap = mp,
      cAddress = addressMap,
      cMemSize = totalSize,
      (!!) = access,
      imAt = imAt,
      reAt = reAt,
      config = config
    }
  where
    (cs, rest) = partition (`Set.member` Set.fromList consecutiveIDs) (IM.keys mp)
    f (addressMap, curSize) nID =
      let (shape, et, node) = retrieveNode nID mp
       in case et of
            R -> (IM.insert nID curSize addressMap, curSize + product shape)
            C -> (IM.insert nID curSize addressMap, curSize + 2 * product shape)
    (memMap, totalSize) = foldl' f (IM.empty, 0) $ cs ++ rest
    addressMap nID
      | Just offset <- IM.lookup nID memMap = offset
      | otherwise = error "Node ID doesn't exist in address map"
    access :: Int -> Text -> Text
    access nID offsetVal =
      let offset
            | offsetVal == "" = ""
            | offsetVal == "0" = ""
            | otherwise = " + " <> offsetVal
       in "ptr[" <> showT (addressMap nID) <> offset <> "]"
    -- Accessor for complex
    reAt nID offsetVal = access nID offsetVal
    imAt nID offsetVal = access nID $ offsetVal <> " + " <> showT (product (retrieveShape nID mp))

-------------------------------------------------------------------------------
evaluating :: CSimpleCodegen -> [NodeID] -> Code
evaluating CSimpleCodegen {..} rootIDs =
  concatMap (fromCCode . genCode) $ topologicalSortManyRoots (cExpressionMap, rootIDs)
  where
    shapeOf nID = retrieveShape nID cExpressionMap
    elementTypeOf nID = retrieveElementType nID cExpressionMap
    addressOf :: NodeID -> Text
    addressOf nID = "(ptr + " <> showT (cAddress nID) <> ")"
    [i, j, k, nooffset] = ["i", "j", "k", "0"]
    len nID = product (retrieveShape nID cExpressionMap)
    complexAt arg i = "(" <> (arg `reAt` i) <> " + " <> (arg `imAt` i) <> " * I)"
    genCode :: NodeID -> CCode
    genCode n =
      let (shape, et, op) = retrieveNode n cExpressionMap
       in case op of
            Var _ -> Empty
            Param _ -> Empty
            Const val -> for i (len n) [Assign (n !! i) (showT val)]
            Sum args
              | et == R ->
                let sumAt i = T.intercalate " + " $ map (!! i) args
                 in for i (len n) [Assign (n !! i) (sumAt i)]
              | et == C ->
                let sumReAt i = T.intercalate " + " $ map (`reAt` i) args
                    sumImAt i = T.intercalate " + " $ map (`imAt` i) args
                 in for i (len n) $
                      [ Assign (n `reAt` i) (sumReAt i),
                        Assign (n `imAt` i) (sumImAt i)
                      ]
            Mul args
              | et == R ->
                let prodAt i = T.intercalate " * " $ map (!! i) args
                 in for i (len n) [Assign (n !! i) (prodAt i)]
              | et == C ->
                let prodAt i = T.intercalate " * " $ map (`complexAt` i) args
                 in for i (len n) $
                      [ Assign "double complex res" (prodAt i),
                        Assign (n `reAt` i) "creal(res)",
                        Assign (n `imAt` i) "cimag(res)"
                      ]
            Power x arg
              | et == R ->
                for i (len n) $
                  [ Assign (n !! i) (fun "pow" [arg !! i, showT x])
                  ]
              | et == C ->
                for i (len n) $
                  [ Assign "double complex res" (fun "cpow" [arg `complexAt` i, showT x]),
                    Assign (n `reAt` i) "creal(res)",
                    Assign (n `imAt` i) "cimag(res)"
                  ]
            Neg arg
              | et == R -> for i (len n) [Assign (n !! i) ("-" <> (arg !! i))]
              | et == C ->
                for i (len n) $
                  [ Assign (n `reAt` i) ("-" <> (arg `reAt` i)),
                    Assign (n `imAt` i) ("-" <> (arg `imAt` i))
                  ]
            Scale scalar arg
              | et == R -> for i (len n) [Assign (n !! i) ((scalar !! nooffset) <> "*" <> (arg !! i))]
              | et == C,
                retrieveElementType scalar cExpressionMap == R ->
                for i (len n) $
                  [ Assign (n `reAt` i) ((scalar !! nooffset) <> "*" <> (arg `reAt` i)),
                    Assign (n `imAt` i) ((scalar !! nooffset) <> "*" <> (arg `imAt` i))
                  ]
              | et == C,
                retrieveElementType scalar cExpressionMap == C ->
                for i (len n) $
                  [ Assign "double complex res" ((scalar `complexAt` nooffset) <> "*" <> (arg `complexAt` i)),
                    Assign (n `reAt` i) "creal(res)",
                    Assign (n `imAt` i) "cimag(res)"
                  ]
            Div arg1 arg2
              | et == R ->
                for i (len n) $
                  [Assign (n !! i) ((arg1 !! i) <> " / " <> (arg2 !! i))]
              | et == C ->
                for i (len n) $
                  [ Assign "double complex res" ((arg1 `complexAt` i) <> " / " <> (arg2 `complexAt` i)),
                    Assign (n `reAt` i) "creal(res)",
                    Assign (n `imAt` i) "cimag(res)"
                  ]
            Sqrt arg -> for i (len n) [Assign (n !! i) (fun "sqrt" [arg !! i])]
            Sin arg -> for i (len n) [Assign (n !! i) (fun "sin" [arg !! i])]
            Cos arg -> for i (len n) [Assign (n !! i) (fun "cos" [arg !! i])]
            Tan arg -> for i (len n) [Assign (n !! i) (fun "tan" [arg !! i])]
            Exp arg -> for i (len n) [Assign (n !! i) (fun "exp" [arg !! i])]
            Log arg -> for i (len n) [Assign (n !! i) (fun "log" [arg !! i])]
            Sinh arg -> for i (len n) [Assign (n !! i) (fun "sinh" [arg !! i])]
            Cosh arg -> for i (len n) [Assign (n !! i) (fun "cosh" [arg !! i])]
            Tanh arg -> for i (len n) [Assign (n !! i) (fun "tanh" [arg !! i])]
            Asin arg -> for i (len n) [Assign (n !! i) (fun "asin" [arg !! i])]
            Acos arg -> for i (len n) [Assign (n !! i) (fun "acos" [arg !! i])]
            Atan arg -> for i (len n) [Assign (n !! i) (fun "atan" [arg !! i])]
            Asinh arg -> for i (len n) [Assign (n !! i) (fun "asinh" [arg !! i])]
            Acosh arg -> for i (len n) [Assign (n !! i) (fun "acosh" [arg !! i])]
            Atanh arg -> for i (len n) [Assign (n !! i) (fun "atanh" [arg !! i])]
            RealImag arg1 arg2 ->
              for i (len n) $
                [ Assign (n `reAt` i) (arg1 !! i),
                  Assign (n `imAt` i) (arg2 !! i)
                ]
            RealPart arg -> for i (len n) [Assign (n !! i) (arg `reAt` i)]
            ImagPart arg -> for i (len n) [Assign (n !! i) (arg `imAt` i)]
            Conjugate arg ->
              for i (len n) $
                [ Assign (n `reAt` i) (arg `reAt` i),
                  Assign (n `reAt` i) ("-" <> (arg `imAt` i))
                ]
            InnerProd arg1 arg2
              | et == R && null (shapeOf arg1) -> Assign (n !! nooffset) ((arg1 !! nooffset) <> " * " <> (arg2 !! nooffset))
              | et == R ->
                Scoped
                  [ Assign "double acc" "0",
                    for i (len arg1) [Assign "acc" ("acc + " <> ((arg1 !! i) <> "*" <> (arg2 !! i)))],
                    Assign (n !! nooffset) "acc"
                  ]
              -- Conjugate the second operand
              | et == C && null (shapeOf arg1) ->
                Scoped
                  [ Assign "double complex res" ((arg1 `complexAt` nooffset) <> " * " <> fun "conj" [arg2 `complexAt` nooffset]),
                    Assign (n `reAt` nooffset) "creal(res)",
                    Assign (n `imAt` nooffset) "cimag(res)"
                  ]
              | et == C ->
                Scoped
                  [ Assign "double complex acc" "0 + 0 * I",
                    for i (len arg1) [Assign "acc" ("acc + " <> ((arg1 `complexAt` i) <> " * " <> fun "conj" [arg2 `complexAt` i]))],
                    Assign (n `reAt` nooffset) "creal(acc)",
                    Assign (n `imAt` nooffset) "cimag(acc)"
                  ]
            Piecewise marks condition branches ->
              let m : ms = marks
                  Just (b : bs, lst) = viewR branches
                  elseifEach (m, b) =
                    elseif_
                      ((condition !! i) <> " <= " <> showT m)
                      ( if et == R
                          then [Assign (n !! i) (b !! i)]
                          else
                            [ Assign (n `reAt` i) (b `reAt` i),
                              Assign (n `imAt` i) (b `imAt` i)
                            ]
                      )
               in for i (len n) $
                    [ if_
                        ((condition !! i) <> " <= " <> showT m)
                        ( if et == R
                            then [Assign (n !! i) (b !! i)]
                            else
                              [ Assign (n `reAt` i) (b `reAt` i),
                                Assign (n `imAt` i) (b `imAt` i)
                              ]
                        )
                    ]
                      ++ map elseifEach (zip ms bs)
                      ++ [ else_
                             ( if et == R
                                 then [Assign (n !! i) (lst !! i)]
                                 else
                                   [ Assign (n `reAt` i) (lst `reAt` i),
                                     Assign (n `imAt` i) (lst `imAt` i)
                                   ]
                             )
                         ]
            Rotate [amount] arg ->
              let [size] = shape
               in for i size $
                    [ Assign "int origin" ("(i - " <> showT amount <> " + " <> showT size <> ") % " <> showT size)
                    ]
                      ++ ( if et == R
                             then [Assign (n !! i) (arg !! "origin")]
                             else
                               [ Assign (n `reAt` i) (arg `reAt` "origin"),
                                 Assign (n `imAt` i) (arg `imAt` "origin")
                               ]
                         )
            Rotate [amount1, amount2] arg ->
              let [size1, size2] = shape
               in for i size1 $
                    [ for j size2 $
                        [ Assign "int ai" ("(i - " <> showT amount1 <> " + " <> showT size1 <> ") % " <> showT size1),
                          Assign "int aj" ("(j - " <> showT amount2 <> " + " <> showT size2 <> ") % " <> showT size2),
                          Assign "int cur" ("i * " <> showT size2 <> " + j"),
                          Assign "int origin" ("ai * " <> showT size2 <> " + aj")
                        ]
                          ++ ( if et == R
                                 then [Assign (n !! "cur") (arg !! "origin")]
                                 else
                                   [ Assign (n `reAt` "cur") (arg `reAt` "origin"),
                                     Assign (n `imAt` "cur") (arg `imAt` "origin")
                                   ]
                             )
                    ]
            Rotate [amount1, amount2, amount3] arg ->
              let [size1, size2, size3] = shape
               in for i size1 $
                    [ for j size2 $
                        [ for k size3 $
                            [ Assign "int ai" ("(i - " <> showT amount1 <> " + " <> showT size1 <> ") % " <> showT size1),
                              Assign "int aj" ("(j - " <> showT amount2 <> " + " <> showT size2 <> ") % " <> showT size2),
                              Assign "int ak" ("(j - " <> showT amount3 <> " + " <> showT size3 <> ") % " <> showT size3),
                              Assign "int cur" ("i * " <> showT size2 <> "*" <> showT size3 <> " + j * " <> showT size3 <> " + k"),
                              Assign "int origin" ("ai * " <> showT size2 <> "*" <> showT size3 <> " + aj * " <> showT size3 <> " + ak")
                            ]
                              ++ ( if et == R
                                     then [Assign (n !! "cur") (arg !! "offset")]
                                     else
                                       [ Assign (n `reAt` "cur") (arg `reAt` "offset"),
                                         Assign (n `imAt` "cur") (arg `imAt` "offset")
                                       ]
                                 )
                        ]
                    ]
            FT arg ->
              case shape of
                [size] -> Statement (fun "dft_1d" [showT size, addressOf arg, addressOf n, "FFTW_FORWARD"])
                [size1, size2] -> Statement (fun "dft_2d" [showT size1, showT size2, addressOf arg, addressOf n, "FFTW_FORWARD"])
            IFT arg ->
              case shape of
                [size] -> Statement (fun "dft_1d" [showT size, addressOf arg, addressOf n, "FFTW_BACKWARD"])
                [size1, size2] -> Statement (fun "dft_2d" [showT size1, showT size2, addressOf arg, addressOf n, "FFTW_BACKWARD"])
            Project dss arg ->
              case (dss, retrieveShape arg cExpressionMap) of
                ([ds], [size]) ->
                  Scoped $
                    [ Assign "int nxt" "0",
                      forRange i (toRange ds size) $
                        if et == R
                          then
                            [ Assign "int origin" ("i % " <> showT size),
                              Assign (n !! "nxt") (arg !! "origin"),
                              Assign "nxt" "nxt + 1"
                            ]
                          else
                            [ Assign "int origin" ("i % " <> showT size),
                              Assign (n `reAt` "nxt") (arg `reAt` "origin"),
                              Assign (n `imAt` "nxt") (arg `imAt` "origin"),
                              Assign "nxt" "nxt + 1"
                            ]
                    ]
                ([ds1, ds2], [size1, size2]) ->
                  Scoped $
                    [ Assign "int nxt" "0",
                      forRange i (toRange ds1 size1) $
                        [ forRange j (toRange ds2 size2) $
                            if et == R
                              then
                                [ Assign "int ai" ("i % " <> showT size1),
                                  Assign "int aj" ("j % " <> showT size2),
                                  Assign "int origin" ("ai * " <> showT size2 <> " + aj"),
                                  Assign (n !! "nxt") (arg !! "origin"),
                                  Assign "nxt" "nxt + 1"
                                ]
                              else
                                [ Assign "int ai" ("i % " <> showT size1),
                                  Assign "int aj" ("j % " <> showT size2),
                                  Assign "int origin" ("ai * " <> showT size2 <> " + aj"),
                                  Assign (n `reAt` "nxt") (arg `reAt` "origin"),
                                  Assign (n `imAt` "nxt") (arg `imAt` "origin"),
                                  Assign "nxt" "nxt + 1"
                                ]
                        ]
                    ]
                ([ds1, ds2, ds3], [size1, size2, size3]) ->
                  Scoped $
                    [ Assign "int nxt" "0",
                      forRange i (toRange ds1 size1) $
                        [ forRange j (toRange ds2 size2) $
                            [ forRange k (toRange ds3 size3) $
                                if et == R
                                  then
                                    [ Assign "int ai" ("i % " <> showT size1),
                                      Assign "int aj" ("j % " <> showT size2),
                                      Assign "int ak" ("k % " <> showT size3),
                                      Assign "int origin" ("ai * " <> showT size2 <> "*" <> showT size3 <> " + aj * " <> showT size3 <> " + ak"),
                                      Assign (n !! "nxt") (arg !! "origin"),
                                      Assign "nxt" "nxt + 1"
                                    ]
                                  else
                                    [ Assign "int ai" ("i % " <> showT size1),
                                      Assign "int aj" ("j % " <> showT size2),
                                      Assign "int ak" ("k % " <> showT size3),
                                      Assign "int origin" ("ai * " <> showT size2 <> "*" <> showT size3 <> " + aj * " <> showT size3 <> " + ak"),
                                      Assign (n `reAt` "nxt") (arg `reAt` "origin"),
                                      Assign (n `imAt` "nxt") (arg `imAt` "origin"),
                                      Assign "nxt" "nxt + 1"
                                    ]
                            ]
                        ]
                    ]
            Inject dss sub base ->
              let copyBase =
                    for i (len n) $
                      if et == R
                        then [Assign (n !! i) (base !! i)]
                        else
                          [ Assign (n `reAt` i) (base `reAt` i),
                            Assign (n `imAt` i) (base `imAt` i)
                          ]
                  injectSub =
                    case (dss, retrieveShape n cExpressionMap) of
                      ([ds], [size]) ->
                        Scoped $
                          [ Assign "int nxt" "0",
                            forRange i (toRange ds size) $
                              if et == R
                                then
                                  [ Assign "int origin" ("i % " <> showT size),
                                    Assign (n !! "origin") (sub !! "nxt"),
                                    Assign "nxt" "nxt + 1"
                                  ]
                                else
                                  [ Assign "int origin" ("i % " <> showT size),
                                    Assign (n `reAt` "origin") (sub `reAt` "nxt"),
                                    Assign (n `imAt` "origin") (sub `imAt` "nxt"),
                                    Assign "nxt" "nxt + 1"
                                  ]
                          ]
                      ([ds1, ds2], [size1, size2]) ->
                        Scoped $
                          [ Assign "int nxt" "0",
                            forRange i (toRange ds1 size1) $
                              [ forRange j (toRange ds2 size2) $
                                  if et == R
                                    then
                                      [ Assign "int ai" ("i % " <> showT size1),
                                        Assign "int aj" ("j % " <> showT size2),
                                        Assign "int origin" ("ai * " <> showT size2 <> " + aj"),
                                        Assign (n !! "origin") (sub !! "nxt"),
                                        Assign "nxt" "nxt + 1"
                                      ]
                                    else
                                      [ Assign "int ai" ("i % " <> showT size1),
                                        Assign "int aj" ("j % " <> showT size2),
                                        Assign "int origin" ("ai * " <> showT size2 <> " + aj"),
                                        Assign (n `reAt` "origin") (sub `reAt` "nxt"),
                                        Assign (n `imAt` "origin") (sub `imAt` "nxt"),
                                        Assign "nxt" "nxt + 1"
                                      ]
                              ]
                          ]
                      ([ds1, ds2, ds3], [size1, size2, size3]) ->
                        Scoped $
                          [ Assign "int nxt" "0",
                            forRange i (toRange ds1 size1) $
                              [ forRange j (toRange ds2 size2) $
                                  [ forRange k (toRange ds3 size3) $
                                      if et == R
                                        then
                                          [ Assign "int ai" ("i % " <> showT size1),
                                            Assign "int aj" ("j % " <> showT size2),
                                            Assign "int ak" ("k % " <> showT size3),
                                            Assign "int origin" ("ai * " <> showT size2 <> "*" <> showT size3 <> " + aj * " <> showT size3 <> " + ak"),
                                            Assign (n !! "origin") (sub !! "nxt"),
                                            Assign "nxt" "nxt + 1"
                                          ]
                                        else
                                          [ Assign "int ai" ("i % " <> showT size1),
                                            Assign "int aj" ("j % " <> showT size2),
                                            Assign "int ak" ("k % " <> showT size3),
                                            Assign "int origin" ("ai * " <> showT size2 <> "*" <> showT size3 <> " + aj * " <> showT size3 <> " + ak"),
                                            Assign (n `reAt` "origin") (sub `reAt` "nxt"),
                                            Assign (n `imAt` "origin") (sub `imAt` "nxt"),
                                            Assign "nxt" "nxt + 1"
                                          ]
                                  ]
                              ]
                          ]
               in Scoped [copyBase, injectSub]
            node -> error $ "Not implemented " ++ show node

-------------------------------------------------------------------------------
instance Codegen CSimpleConfig where
  generateProblemCode :: CSimpleConfig -> Problem -> ValMaps -> GenResult
  generateProblemCode cf@CSimpleConfig {..} Problem {..} valMaps
    | Just errorMsg <- checkError = Invalid errorMsg
    | otherwise = Success $ \folder -> do
      -- If the value is not from file, write all the values into
      -- text files so C code can read them
      let writeVal val filePath = TIO.writeFile filePath $ T.unwords . map showT . valElems $ val
      -- Write values
      forM_ (Map.toList valMaps) $ \(var, val) -> do
        when (valueFromHaskell val) $ do
          let str = T.unwords . map showT . valElems $ val
          TIO.writeFile (folder </> var <.> "txt") str
      -- Write box constraints
      forM_ boxConstraints $ \c -> case c of
        BoxLower var val -> when (valueFromHaskell val) $ writeVal val (folder </> (var <> "_lb.txt"))
        BoxUpper var val -> when (valueFromHaskell val) $ writeVal val (folder </> (var <> "_ub.txt"))
        BoxBetween var (val1, val2) -> do
          when (valueFromHaskell val1) $ writeVal val1 (folder </> (var <> "_lb.txt"))
          when (valueFromHaskell val2) $ writeVal val2 (folder </> (var <> "_ub.txt"))
      -- Write code
      let codes =
            concat
              [ defineStuffs,
                constraintCodes,
                readValsCodes,
                writeResultCodes,
                evaluatingCodes,
                evaluateObjectiveCodes,
                evaluatePartialDerivativesCodes,
                evaluateScalarConstraintsCodes,
                evaluateScalarConstraintsJacobianCodes
              ]
      TIO.writeFile (folder </> "problem.c") $ T.intercalate "\n" codes
    where
      -------------------------------------------------------------------------------
      -- variables
      vars :: [String]
      vars = map varName variables
      -- params
      params :: [String]
      params = map fst $ paramNodesWithId expressionMap
      -- value nodes
      varsAndParams :: [(String, Int)]
      varsAndParams = sortOn fst $ varNodesWithId expressionMap ++ paramNodesWithId expressionMap
      -- get shape of a variable
      variableShape :: String -> Shape
      variableShape name =
        let nId = case find ((== name) . varName) variables of
              Just var -> nodeId var
              _ -> error "not a variable but you're getting it's shape"
         in retrieveShape nId expressionMap
      -------------------------------------------------------------------------------
      variableSizes :: [Int]
      variableSizes = map (product . variableShape . varName) variables
      -------------------------------------------------------------------------------
      checkError :: Maybe String
      checkError
        | Just name <- find (not . (`Map.member` valMaps)) params = Just $ "No value provided for " ++ name
        | otherwise,
          let isOk (var, nId)
                | Just val <- Map.lookup var valMaps = compatible (retrieveShape nId expressionMap) val
                | otherwise = True,
          Just (var, shape) <- find (not . isOk) varsAndParams =
          Just $ "variable " ++ var ++ "is of shape " ++ show shape ++ " but the value provided is not"
        | otherwise = Nothing
      -------------------------------------------------------------------------------
      codegen@CSimpleCodegen {..} = initCodegen cf expressionMap (map nodeId variables)
      variableOffsets = map (cAddress . nodeId) variables
      partialDerivativeOffsets = map (cAddress . partialDerivativeId) variables
      objectiveOffset = cAddress objectiveId
      -- For both variables and values
      readValCodeEach (name, nId)
        | Just val <- Map.lookup name valMaps = generateReadValuesCode (name, product shape) ("ptr + " ++ show offset) val
        | otherwise =
          Scoped
            [ Printf ["Init value for " <> T.pack name <> "is not provided, generate random init for " <> T.pack "name" <> " ...\\n"],
              for "i" (product shape) $
                [Assign ("ptr[" <> showT offset <> "+ i]") ("(double) rand() / RAND_MAX")]
            ]
        where
          offset = cAddress nId
          shape = retrieveShape nId expressionMap
      -------------------------------------------------------------------------------
      writeResultCodeEach :: Variable -> CCode
      writeResultCodeEach variable
        | output == OutputHDF5 =
          Scoped
            [ Printf ["Writing " <> T.pack name <> " to " <> T.pack name <> "_out.h5...\\n"],
              Statement "hid_t file, space, dset",
              Assign ("hsize_t dims[" <> showT (length shape) <> "]") ("{" <> T.intercalate ", " (map showT shape) <> "}"),
              Assign "file" (fun "H5Fcreate" ["\"" <> T.pack name <> "_out.h5\"", "H5F_ACC_TRUNC", "H5P_DEFAULT", "H5P_DEFAULT"]),
              Assign "space" (fun "H5Screate_simple" [showT $ length shape, "dims", "NULL"]),
              Assign "dset" (fun "H5Dcreate" ["file", T.pack name, "H5T_IEEE_F64LE", "space", "H5P_DEFAULT", "H5P_DEFAULT", "H5P_DEFAULT"]),
              Statement (fun "H5Dwrite" ["dset", "H5T_NATIVE_DOUBLE", "H5S_ALL", "H5S_ALL", "H5P_DEFAULT", "ptr + " <> showT offset]),
              Statement "H5Dclose(dset)",
              Statement "H5Sclose(space)",
              Statement "H5Fclose(file"
            ]
        | output == OutputText =
          Scoped $
            [ Printf ["Writing " <> T.pack name <> " to " <> T.pack name <> "_out.txt...\\n"],
              Statement "FILE *file",
              Assign "file" (fun "fopen" ["\"" <> T.pack name <> "_out.txt\"", "\"w\""]),
              for "i" (product shape) $
                [ Statement (fun "fprintf" ["file", "\"%f \"", "ptr[" <> showT offset <> " + i]"])
                ],
              Statement "fclose(file)"
            ]
        where
          nId = nodeId variable
          name = varName variable
          offset = cAddress nId
          shape = retrieveShape nId expressionMap
      -------------------------------------------------------------------------------
      defineStuffs :: Code
      defineStuffs =
        [ "#include <math.h>",
          "#include <stdio.h>",
          "#include <stdlib.h>",
          "#include <time.h>",
          "#include \"hdf5.h\"",
          if containsFTNode expressionMap
            then T.pack fftUtils
            else "",
          "#include <complex.h>",
          "// number of (higher dimensional) variables ",
          "#define NUM_VARIABLES " <> showT (length variables),
          "// number of real variables, because each higher dimensional var is a grid of real variables",
          "#define NUM_ACTUAL_VARIABLES " <> showT (sum variableSizes),
          "#define MEM_SIZE " <> showT cMemSize,
          "// all the actual double variables are allocated",
          "// one after another, starts from here",
          "#define VARS_START_OFFSET " <> showT (cAddress (nodeId . head $ variables)),
          "const char* var_name[NUM_VARIABLES] = {"
            <> (T.intercalate ", " . map (showT . varName) $ variables)
            <> "};",
          "const int var_size[NUM_VARIABLES] = {"
            <> (T.intercalate ", " . map showT $ variableSizes)
            <> "};",
          "const int var_offset[NUM_VARIABLES] = {"
            <> (T.intercalate ", " . map showT $ variableOffsets)
            <> "};",
          "const int partial_derivative_offset[NUM_VARIABLES] = {"
            <> (T.intercalate ", " . map showT $ partialDerivativeOffsets)
            <> "};",
          "const int objective_offset = " <> showT objectiveOffset <> ";",
          "double ptr[MEM_SIZE];"
        ]
      -------------------------------------------------------------------------------
      constraintCodes =
        let varPosition = take (length variableSizes) $ scanl (+) 0 variableSizes
            varWithPos = zip vars varPosition
            getPos name = snd . fromMaybe (error "get starting position variable") . find ((== name) . fst) $ varWithPos
            readUpperBoundCode name val =
              fromCCode $
                generateReadValuesCode
                  ((name ++ "_ub"), product $ variableShape name)
                  ("upper_bound + " ++ show (getPos name))
                  val
            readLowerBoundCode name val =
              fromCCode $
                generateReadValuesCode
                  ((name ++ "_lb"), (product $ variableShape name))
                  ("lower_bound + " ++ show (getPos name))
                  val
            readBounds =
              let readBoundCodeEach cnt =
                    case cnt of
                      BoxUpper name val -> readUpperBoundCode name val
                      BoxLower name val -> readLowerBoundCode name val
                      BoxBetween name (val1, val2) -> readLowerBoundCode name val1 ++ readUpperBoundCode name val2
               in concatMap readBoundCodeEach boxConstraints
            scalarConstraintDefineStuffs =
              [ "#define NUM_SCALAR_CONSTRAINT " <> showT (length scalarConstraints),
                "double sc_lower_bound[NUM_SCALAR_CONSTRAINT];",
                "double sc_upper_bound[NUM_SCALAR_CONSTRAINT];",
                "const int sc_offset[NUM_SCALAR_CONSTRAINT] = {"
                  <> (T.intercalate "," . map (showT . cAddress . constraintValueId) $ scalarConstraints)
                  <> "};",
                "",
                "const int sc_partial_derivative_offset[NUM_SCALAR_CONSTRAINT][NUM_VARIABLES] = {"
                  <> T.intercalate
                    ", "
                    [ "{" <> T.intercalate "," (map (showT . cAddress) . constraintPartialDerivatives $ sc) <> "}"
                      | sc <- scalarConstraints
                    ]
                  <> "};"
              ]
            readBoundScalarConstraints =
              [ "sc_lower_bound[" <> showT i <> "] = " <> d2s val <> ";"
                | (i, val) <- zip [0 ..] $ map constraintLowerBound scalarConstraints
              ]
                <> [ "sc_upper_bound[" <> showT i <> "] = " <> d2s val <> ";"
                     | (i, val) <- zip [0 ..] $ map constraintUpperBound scalarConstraints
                   ]
         in [ "const int bound_pos[NUM_VARIABLES] = {" <> (T.intercalate ", " . map showT $ varPosition) <> "};",
              "double lower_bound[NUM_ACTUAL_VARIABLES];",
              "double upper_bound[NUM_ACTUAL_VARIABLES];"
            ]
              ++ scalarConstraintDefineStuffs
              ++ [ "void read_bounds() {", --
                   "  for (int i = 0; i < NUM_ACTUAL_VARIABLES; i++) {",
                   "    lower_bound[i] = -INFINITY;",
                   "    upper_bound[i] = INFINITY;",
                   "  }"
                 ]
              ++ indent 2 readBounds
              ++ indent 2 readBoundScalarConstraints --
              ++ ["}"]
      -------------------------------------------------------------------------------
      readValsCodes =
        ["void read_values() {"]
          ++ ["  srand(time(NULL));"] --
          ++ scoped (concatMap (fromCCode . readValCodeEach) varsAndParams)
          ++ ["}"] --
          -------------------------------------------------------------------------------
      writeResultCodes =
        ["void write_result()"]
          ++ scoped (concatMap (fromCCode . writeResultCodeEach) variables)
      -------------------------------------------------------------------------------
      evaluatingCodes =
        ["void evaluate_partial_derivatives_and_objective()"]
          ++ scoped (evaluating codegen $ objectiveId : map partialDerivativeId variables)
      -------------------------------------------------------------------------------
      evaluateObjectiveCodes =
        ["void evaluate_objective()"]
          ++ scoped (evaluating codegen [objectiveId])
      -------------------------------------------------------------------------------
      evaluatePartialDerivativesCodes =
        ["void evaluate_partial_derivatives()"]
          ++ scoped (evaluating codegen (map partialDerivativeId variables))
      -------------------------------------------------------------------------------
      evaluateScalarConstraintsCodes =
        ["void evaluate_scalar_constraints()"]
          ++ scoped (evaluating codegen (map constraintValueId scalarConstraints))
      -------------------------------------------------------------------------------
      evaluateScalarConstraintsJacobianCodes =
        ["void evaluate_scalar_constraints_jacobian()"]
          ++ scoped (evaluating codegen (concatMap constraintPartialDerivatives scalarConstraints))

-------------------------------------------------------------------------------

toShapeString :: Shape -> T.Text
toShapeString shape
  | length shape < 3 =
    "{"
      <> (T.intercalate ", " . map showT $ shape <> replicate (3 - length shape) 1)
      <> "}"
  | otherwise = "{" <> (T.intercalate ", " . map showT $ shape) <> "}"

-------------------------------------------------------------------------------

generateReadValuesCode :: (String, Int) -> String -> Val -> CCode
generateReadValuesCode (name, size) address val =
  case val of
    VScalar value -> Scoped [Assign ("*(" <> T.pack address <> ")") (showT value)]
    V1D _ -> readFileText (T.pack name <> ".txt")
    V2D _ -> readFileText (T.pack name <> ".txt")
    V3D _ -> readFileText (T.pack name <> ".txt")
    VFile (TXT filePath) -> readFileText $ T.pack filePath
    VFile (HDF5 filePath dataset) -> readFileHD5 (T.pack filePath) (T.pack dataset)
    VNum value ->
      for "i" size $
        [ Assign ("*(" <> T.pack address <> " + i)") (showT value)
        ]
  where
    readFileText filePath =
      Scoped
        [ Printf ["Reading " <> T.pack name <> " from text file " <> filePath <> " ...\\n"],
          Assign "FILE *fp" ("fopen(\"" <> filePath <> "\", \"r\")"),
          for "i" size $
            [ Statement (fun "fscanf" ["fp", "\"%lf\"", T.pack address <> " + i"])
            ],
          Statement (fun "fclose" ["fp"])
        ]
    readFileHD5 filePath dataset =
      Scoped
        [ Printf ["Reading " <> T.pack name <> " from HDF5 file in dataset " <> dataset <> " from " <> filePath <> " ...\\n"],
          Statement "hid_t file, dset",
          Assign "file" (fun "H5Fopen" [filePath, "H5F_ACC_RDONLY", "H5P_DEFAULT"]),
          Assign "dset" (fun "H5Dopen" ["file", dataset, "H5P_DEFAULT"]),
          Statement (fun "H5Dread" ["dset", "H5T_NATIVE_DOUBLE", "H5S_ALL", "H5S_ALL", "H5P_DEFAULT", T.pack address]),
          Statement "H5Fclose (file)",
          Statement "H5Dclose (dset)"
        ]
