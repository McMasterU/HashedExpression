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
module HashedExpression.Codegen.CSimple1 where

import Control.Monad (forM_, when)
import Data.Array (indices, (!))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (find, foldl', intercalate, partition, sortOn, tails)
import Data.List.HT (viewR)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.String.Interpolate
import qualified Data.String.Interpolate as I
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

fromCCode :: CCode -> Code
fromCCode c = case c of
  Assign lhs rhs -> [lhs <> " = " <> rhs <> ";"]
  Statement ss -> [ss <> ";"]
  Control control codes -> [control <> "{"] ++ concatMap fromCCode codes ++ ["}"]
  Empty -> []
  Scoped codes -> ["{"] ++ concatMap fromCCode codes ++ ["}"]


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
  Control
    ( "for ("
        <> (iter <> " = 0; ")
        <> (iter <> " < " <> showT bound <> "; ")
        <> (iter <> "++")
        <> ")"
    )
    codes

if_ :: Text -> [CCode] -> CCode
if_ condition = Control ("if (" <> condition <> ")")

elseif_ :: Text -> [CCode] -> CCode
elseif_ condition = Control ("else if (" <> condition <> ")")

else_ :: [CCode] -> CCode
else_ = Control "else"

forRange :: Text -> (Int, Int, Int) -> [CCode] -> CCode
forRange iter (start, end, step) codes =
  Control
    ( "for ("
        <> (iter <> " = " <> showT start <> ";")
        <> (iter <> " <= " <> showT end <> "; ")
        <> (iter <> " += " <> showT step)
        <> ")"
    )
    codes

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
       in [i|ptr[#{addressMap nID}#{offset}]|]
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
    addressOf nID = [I.i|(ptr + #{cAddress nID})|]
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
                    Assign (n `reAt` nooffset) "creal(res)",
                    Assign (n `imAt` nooffset) "cimag(res)"
                  ]
            Piecewise marks condition branches ->
              let m : ms = map showT marks
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
