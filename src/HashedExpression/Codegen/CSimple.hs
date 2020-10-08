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
import Data.List (find, foldl', partition, sortOn)
import Data.List.HT (viewR)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import HashedExpression.Codegen (Code, Codegen (..), indent)
import HashedExpression.Embed 
import HashedExpression.Internal
import HashedExpression.Internal.Base (ElementType (..), ExpressionMap, NodeID (..), Op (..), Shape)
import HashedExpression.Internal.Node (retrieveElementType, retrieveNode, retrieveOp, retrieveShape)
import HashedExpression.Problem
import HashedExpression.Utils
import HashedExpression.Value
import System.FilePath
import Prelude hiding ((!!))
-------------------------------------------------------------------------------

data DataOutput = OutputText | OutputHDF5 deriving (Eq, Show)

-- | Generate simple C code
data CSimpleConfig = CSimpleConfig
  { output :: DataOutput,
    maxIteration :: Maybe Int
  }
  deriving (Eq, Show)

-- | Offset w.r.t "ptr"
data Address
  = AddressReal Int
  | AddressComplex Int

-- | e.g: i, j, k
type Index = Text

data CSimpleCodegen = CSimpleCodegen
  { cExpressionMap :: ExpressionMap,
    cAddress :: NodeID -> Address,
    totalReal :: Int,
    totalComplex :: Int,
    (!!) :: NodeID -> Index -> Text,
    config :: CSimpleConfig
  }

infix 1 :=

data CCode
  = Text := Text
  | Statement Text
  | Control Text [CCode]
  | Empty
  | Scoped [CCode]
  | Printf [Text]

codeCToText :: CCode -> Text
codeCToText = T.intercalate "\n" . fromCCode

fromCCode :: CCode -> Code
fromCCode c = case c of
  (lhs := rhs) -> [lhs <> " = " <> rhs <> ";"]
  Statement ss -> [ss <> ";"]
  Control control codes -> control : scoped (concatMap fromCCode codes)
  Empty -> []
  Scoped codes -> scoped (concatMap fromCCode codes)
  Printf [] -> []
  Printf (x : xs) -> ["printf(" <> T.intercalate ", " (ttq x : xs) <> ");"]

-- | Helpers for code generation
scoped :: Code -> Code
scoped codes = ["{"] ++ indent 2 codes ++ ["}"]

-------------------------------------------------------------------------------

d2s :: Double -> Text
d2s val
  | val == ninf = "-INFINITY"
  | val == inf = "INFINITY"
  | otherwise = tt val

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
            <> (iter <> " < " <> tt bound <> "; ")
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
            <> (iter <> " = " <> tt start <> ";")
            <> (iter <> " <= " <> tt end <> "; ")
            <> (iter <> " += " <> tt step)
            <> ")"
        )
        codes
    ]

initCodegen :: CSimpleConfig -> ExpressionMap -> [NodeID] -> CSimpleCodegen
initCodegen config mp variableIDs =
  CSimpleCodegen
    { cExpressionMap = mp,
      cAddress = addressMap,
      (!!) = access,
      totalReal = totalSizeReal,
      totalComplex = totalSizeComplex,
      config = config
    }
  where
    (cs, rest) = partition (`Set.member` Set.fromList variableIDs) $ nodeIDs mp
    f (addressMap, curSizeReal, curSizeComplex) nID =
      let (shape, et, op) = retrieveNode nID mp
       in case (op, et) of
            (Coerce {}, _) -> (addressMap, curSizeReal, curSizeComplex)
            (_, R) -> (Map.insert nID (AddressReal curSizeReal) addressMap, curSizeReal + product shape, curSizeComplex)
            (_, C) -> (Map.insert nID (AddressComplex curSizeComplex) addressMap, curSizeReal, curSizeComplex + product shape)
    (memMap, totalSizeReal, totalSizeComplex) = foldl' f (Map.empty, 0, 0) $ cs ++ rest
    addressMap nID
      | Just offset <- Map.lookup nID memMap = offset
      | otherwise = error "Node ID doesn't exist in address map"
    access :: NodeID -> Text -> Text
    access nID offsetVal
      | Coerce _ from <- retrieveOp nID mp = access from offsetVal
      | otherwise =
        let offset
              | offsetVal == "" = ""
              | offsetVal == "0" = ""
              | otherwise = " + " <> offsetVal
         in case addressMap nID of
              AddressReal i -> "ptr[" <> tt i <> offset <> "]"
              AddressComplex i -> "ptr_c[" <> tt i <> offset <> "]"

---------------------------------------------------------------------------------
evaluating :: CSimpleCodegen -> [NodeID] -> Text
evaluating CSimpleCodegen {..} rootIDs =
  codeCToText $ Scoped $ map genCode $ topologicalSortManyRoots (cExpressionMap, rootIDs)
  where
    shapeOf nID = retrieveShape nID cExpressionMap
    addressOf :: NodeID -> Text
    addressOf nID = case cAddress nID of
      AddressReal i -> "(ptr + " <> tt i <> ")"
      AddressComplex i -> "(ptr_c + " <> tt i <> ")"
    [i, j, k, nooffset] = ["i", "j", "k", "0"]
    len nID = product (retrieveShape nID cExpressionMap)
    --    complexAt arg i = "(" <> (arg `reAt` i) <> " + " <> (arg `imAt` i) <> " * I)"
    genCode :: NodeID -> CCode
    genCode n =
      let (shape, et, op) = retrieveNode n cExpressionMap
       in case op of
            Var _ -> Empty
            Param _ -> Empty
            Const val -> for i (len n) [(n !! i) := tt val]
            Sum args ->
              let sumAt i = T.intercalate " + " $ map (!! i) args
               in for i (len n) [(n !! i) := sumAt i]
            Mul args ->
              let prodAt i = T.intercalate " * " $ map (!! i) args
               in for i (len n) [(n !! i) := prodAt i]
            Power x arg
              | et == R ->
                for i (len n) [(n !! i) := fun "pow" [arg !! i, tt x]]
              | et == C ->
                for i (len n) [(n !! i) := fun "cpow" [arg !! i, tt x]]
            Neg arg ->
              for i (len n) [(n !! i) := ("-" <> (arg !! i))]
            Scale scalar arg ->
              for i (len n) [(n !! i) := ((scalar !! nooffset) <> "*" <> (arg !! i))]
            Div arg1 arg2 ->
              for i (len n) [(n !! i) := ((arg1 !! i) <> " / " <> (arg2 !! i))]
            Sqrt arg -> for i (len n) [(n !! i) := fun "sqrt" [arg !! i]]
            Sin arg -> for i (len n) [(n !! i) := fun "sin" [arg !! i]]
            Cos arg -> for i (len n) [(n !! i) := fun "cos" [arg !! i]]
            Tan arg -> for i (len n) [(n !! i) := fun "tan" [arg !! i]]
            Exp arg -> for i (len n) [(n !! i) := fun "exp" [arg !! i]]
            Log arg -> for i (len n) [(n !! i) := fun "log" [arg !! i]]
            Sinh arg -> for i (len n) [(n !! i) := fun "sinh" [arg !! i]]
            Cosh arg -> for i (len n) [(n !! i) := fun "cosh" [arg !! i]]
            Tanh arg -> for i (len n) [(n !! i) := fun "tanh" [arg !! i]]
            Asin arg -> for i (len n) [(n !! i) := fun "asin" [arg !! i]]
            Acos arg -> for i (len n) [(n !! i) := fun "acos" [arg !! i]]
            Atan arg -> for i (len n) [(n !! i) := fun "atan" [arg !! i]]
            Asinh arg -> for i (len n) [(n !! i) := fun "asinh" [arg !! i]]
            Acosh arg -> for i (len n) [(n !! i) := fun "acosh" [arg !! i]]
            Atanh arg -> for i (len n) [(n !! i) := fun "atanh" [arg !! i]]
            RealImag arg1 arg2 ->
              for i (len n) $
                [ (n !! i) := ((arg1 !! i) <> " + " <> (arg2 !! i) <> " * I")
                ]
            RealPart arg -> for i (len n) [(n !! i) := fun "creal" [arg !! i]]
            ImagPart arg -> for i (len n) [(n !! i) := fun "cimag" [arg !! i]]
            Conjugate arg ->
              for i (len n) $
                [ (n !! i) := fun "conj" [arg !! i]
                ]
            InnerProd arg1 arg2
              | et == R && null (shapeOf arg1) -> (n !! nooffset) := ((arg1 !! nooffset) <> " * " <> (arg2 !! nooffset))
              | et == C && null (shapeOf arg1) -> (n !! nooffset) := ((arg1 !! nooffset) <> " * " <> fun "conj" [arg2 !! nooffset])
              | et == R ->
                Scoped
                  [ "double acc" := "0",
                    for i (len arg1) ["acc" := ("acc + " <> ((arg1 !! i) <> "*" <> (arg2 !! i)))],
                    (n !! nooffset) := "acc"
                  ]
              | et == C ->
                Scoped
                  [ "double complex acc" := "0 + 0 * I",
                    for i (len arg1) ["acc" := ("acc + " <> ((arg1 !! i) <> " * " <> fun "conj" [arg2 !! i]))],
                    (n !! nooffset) := "acc"
                  ]
            Piecewise marks condition branches ->
              let m : ms = marks
                  Just (b : bs, lst) = viewR branches
                  elseifEach (m, b) =
                    elseif_
                      ((condition !! i) <> " <= " <> tt m)
                      [(n !! i) := (b !! i)]
               in for i (len n) $
                    [ if_
                        ((condition !! i) <> " <= " <> tt m)
                        [(n !! i) := (b !! i)]
                    ]
                      ++ map elseifEach (zip ms bs)
                      ++ [ else_ [(n !! i) := (lst !! i)]
                         ]
            Rotate [amount] arg ->
              let [size] = shape
               in for i size $
                    [ "int origin" := ("(i - " <> tt amount <> " + " <> tt size <> ") % " <> tt size),
                      (n !! i) := (arg !! "origin")
                    ]
            Rotate [amount1, amount2] arg ->
              let [size1, size2] = shape
               in for i size1 $
                    [ for j size2 $
                        [ "int ai" := ("(i - " <> tt amount1 <> " + " <> tt size1 <> ") % " <> tt size1),
                          "int aj" := ("(j - " <> tt amount2 <> " + " <> tt size2 <> ") % " <> tt size2),
                          "int cur" := ("i * " <> tt size2 <> " + j"),
                          "int origin" := ("ai * " <> tt size2 <> " + aj"),
                          (n !! "cur") := (arg !! "origin")
                        ]
                    ]
            Rotate [amount1, amount2, amount3] arg ->
              let [size1, size2, size3] = shape
               in for i size1 $
                    [ for j size2 $
                        [ for k size3 $
                            [ "int ai" := ("(i - " <> tt amount1 <> " + " <> tt size1 <> ") % " <> tt size1),
                              "int aj" := ("(j - " <> tt amount2 <> " + " <> tt size2 <> ") % " <> tt size2),
                              "int ak" := ("(j - " <> tt amount3 <> " + " <> tt size3 <> ") % " <> tt size3),
                              "int cur" := ("i * " <> tt size2 <> "*" <> tt size3 <> " + j * " <> tt size3 <> " + k"),
                              "int origin" := ("ai * " <> tt size2 <> "*" <> tt size3 <> " + aj * " <> tt size3 <> " + ak"),
                              (n !! "cur") := (arg !! "offset")
                            ]
                        ]
                    ]
            FT arg ->
              case shape of
                [] -> (n !! nooffset) := (arg !! nooffset)
                [size] -> Statement (fun "dft_1d" [tt size, addressOf arg, addressOf n, "FFTW_FORWARD"])
                [size1, size2] -> Statement (fun "dft_2d" [tt size1, tt size2, addressOf arg, addressOf n, "FFTW_FORWARD"])
            IFT arg ->
              case shape of
                [] -> (n !! nooffset) := (arg !! nooffset)
                [size] -> Statement (fun "dft_1d" [tt size, addressOf arg, addressOf n, "FFTW_BACKWARD"])
                [size1, size2] -> Statement (fun "dft_2d" [tt size1, tt size2, addressOf arg, addressOf n, "FFTW_BACKWARD"])
            Project dss arg ->
              case (dss, retrieveShape arg cExpressionMap) of
                ([ds], [size]) ->
                  Scoped $
                    [ "int nxt" := "0",
                      forRange i (toRange ds size) $
                        [ "int origin" := ("i % " <> tt size),
                          (n !! "nxt") := (arg !! "origin"),
                          "nxt" := "nxt + 1"
                        ]
                    ]
                ([ds1, ds2], [size1, size2]) ->
                  Scoped $
                    [ "int nxt" := "0",
                      forRange i (toRange ds1 size1) $
                        [ forRange j (toRange ds2 size2) $
                            [ "int ai" := ("i % " <> tt size1),
                              "int aj" := ("j % " <> tt size2),
                              "int origin" := ("ai * " <> tt size2 <> " + aj"),
                              (n !! "nxt") := (arg !! "origin"),
                              "nxt" := "nxt + 1"
                            ]
                        ]
                    ]
                ([ds1, ds2, ds3], [size1, size2, size3]) ->
                  Scoped $
                    [ "int nxt" := "0",
                      forRange i (toRange ds1 size1) $
                        [ forRange j (toRange ds2 size2) $
                            [ forRange k (toRange ds3 size3) $
                                [ "int ai" := ("i % " <> tt size1),
                                  "int aj" := ("j % " <> tt size2),
                                  "int ak" := ("k % " <> tt size3),
                                  "int origin" := ("ai * " <> tt size2 <> "*" <> tt size3 <> " + aj * " <> tt size3 <> " + ak"),
                                  (n !! "nxt") := (arg !! "origin"),
                                  "nxt" := "nxt + 1"
                                ]
                            ]
                        ]
                    ]
            Inject dss sub base ->
              let copyBase =
                    for i (len n) $
                      [(n !! i) := (base !! i)]
                  injectSub =
                    case (dss, retrieveShape n cExpressionMap) of
                      ([ds], [size]) ->
                        Scoped $
                          [ "int nxt" := "0",
                            forRange i (toRange ds size) $
                              [ "int origin" := ("i % " <> tt size),
                                (n !! "origin") := (sub !! "nxt"),
                                "nxt" := "nxt + 1"
                              ]
                          ]
                      ([ds1, ds2], [size1, size2]) ->
                        Scoped $
                          [ "int nxt" := "0",
                            forRange i (toRange ds1 size1) $
                              [ forRange j (toRange ds2 size2) $
                                  [ "int ai" := ("i % " <> tt size1),
                                    "int aj" := ("j % " <> tt size2),
                                    "int origin" := ("ai * " <> tt size2 <> " + aj"),
                                    (n !! "origin") := (sub !! "nxt"),
                                    "nxt" := "nxt + 1"
                                  ]
                              ]
                          ]
                      ([ds1, ds2, ds3], [size1, size2, size3]) ->
                        Scoped $
                          [ "int nxt" := "0",
                            forRange i (toRange ds1 size1) $
                              [ forRange j (toRange ds2 size2) $
                                  [ forRange k (toRange ds3 size3) $
                                      [ "int ai" := ("i % " <> tt size1),
                                        "int aj" := ("j % " <> tt size2),
                                        "int ak" := ("k % " <> tt size3),
                                        "int origin" := ("ai * " <> tt size2 <> "*" <> tt size3 <> " + aj * " <> tt size3 <> " + ak"),
                                        (n !! "origin") := (sub !! "nxt"),
                                        "nxt" := "nxt + 1"
                                      ]
                                  ]
                              ]
                          ]
               in Scoped [copyBase, injectSub]
            MatMul x y ->
              case (retrieveShape x cExpressionMap, retrieveShape y cExpressionMap) of
                ([size1, size2], [_size2]) ->
                  for i size1 $
                    [ if et == R
                        then "double acc" := "0"
                        else "double complex acc" := "0",
                      for j size2 $
                        [ "int ij" := ("i * " <> tt size2 <> " + j"),
                          "acc" := ("acc + " <> (x !! "ij") <> " * " <> (y !! j))
                        ],
                      (n !! i) := "acc"
                    ]
                ([size1, size2], [_size2, size3]) ->
                  for i size1 $
                    [ for j size3 $
                        [ if et == R
                            then "double acc" := "0"
                            else "double complex acc" := "0",
                          for k size2 $
                            [ "int ik" := ("i * " <> tt size2 <> " + k"),
                              "int kj" := ("k * " <> tt size3 <> " + j"),
                              "acc" := ("acc + " <> (x !! "ik") <> " * " <> (y !! "kj"))
                            ],
                          "int ij" := ("i * " <> tt size3 <> " + j"),
                          (n !! "ij") := "acc"
                        ]
                    ]
            Transpose x -> case retrieveShape x cExpressionMap of
              [size1, size2] ->
                for i size2 $
                  [ for j size1 $
                      [ "int ij" := ("i * " <> tt size1 <> " + j"),
                        "int ji" := ("j * " <> tt size2 <> " + i"),
                        (n !! "ij") := (x !! "ji")
                      ]
                  ]
            Coerce {} -> Empty
            node -> error $ "Not implemented " ++ show node

--
---------------------------------------------------------------------------------
instance Codegen CSimpleConfig where
  generateProblemCode :: CSimpleConfig -> Problem -> ValMap -> Either String (String -> IO ())
  generateProblemCode cf@CSimpleConfig {..} Problem {..} valMap
    | Just errorMsg <- checkError = Left errorMsg
    | otherwise = Right $ \folder -> do
      let writeVal val filePath = TIO.writeFile filePath $ T.unwords . map tt . valElems $ val
      -- If the value is not from file, write all the values into
      -- text files so C code can read them
      -- 1. Values
      forM_ (Map.toList valMap) $ \(var, val) -> do
        when (valueFromHaskell val) $ do
          let str = T.unwords . map tt . valElems $ val
          TIO.writeFile (folder </> var <.> "txt") str
      -- 2. Box constraints
      forM_ boxConstraints $ \c -> case c of
        BoxLower var val -> when (valueFromHaskell val) $ writeVal val (folder </> (var <> "_lb.txt"))
        BoxUpper var val -> when (valueFromHaskell val) $ writeVal val (folder </> (var <> "_ub.txt"))
        BoxBetween var (val1, val2) -> do
          when (valueFromHaskell val1) $ writeVal val1 (folder </> (var <> "_lb.txt"))
          when (valueFromHaskell val2) $ writeVal val2 (folder </> (var <> "_ub.txt"))
      let auxMap = Map.fromList $ map (\var -> (varName var, var)) variables
          variableByName name = fromJust $ Map.lookup name auxMap
      let codegen@CSimpleCodegen {..} = initCodegen cf expressionMap (map nodeId variables)
          getShape nID = retrieveShape nID cExpressionMap
          -------------------------------------------------------------------------------------------------
          numHigherOrderVariables = length variables
          varStartOffset = addressReal . nodeId . head $ variables
          numScalarConstraints = length scalarConstraints
          varNames = map varName variables
          varSizes = map (product . getShape . nodeId) variables
          numActualVariables = sum varSizes
          varNameWithStatingPosition = Map.fromList $ zip varNames $ take (length varSizes) $ scanl (+) 0 varSizes
          startingPositionByName name = fromJust $ Map.lookup name varNameWithStatingPosition 
          varOffsets = map (addressReal . nodeId) variables
          partialDerivativeOffsets = map (addressReal . partialDerivativeId) variables
          objectiveOffset = addressReal objectiveId
          scalarConstraintOffsets = map (addressReal . constraintValueId) scalarConstraints
          scalarConstraintPartialDerivativeOffsets = map (map addressReal . constraintPartialDerivatives) scalarConstraints
          readBounds =
              let 
                  readUpperBoundCode name val =
                    generateReadValuesCode
                      ((name <> "_ub"), product . getShape . nodeId . variableByName $ name)
                      ("upper_bound + " <> show (startingPositionByName name))
                      val
                  readLowerBoundCode name val =
                    generateReadValuesCode
                      ((name <> "_lb"), product . getShape . nodeId . variableByName $ name)
                      ("lower_bound + " <> show (startingPositionByName name))
                      val
                  readBoundCodeEach cnt =
                    case cnt of
                      BoxUpper name val -> readUpperBoundCode name val
                      BoxLower name val -> readLowerBoundCode name val
                      BoxBetween name (val1, val2) -> 
                          readLowerBoundCode name val1 <> 
                          "\n" <> 
                          readUpperBoundCode name val2
               in T.intercalate "\n" $ map readBoundCodeEach boxConstraints
          readBoundScalarConstraints = T.intercalate "\n" $ map readBoundEach $ zip [0..] scalarConstraints
            where 
              readBoundEach :: (Int, ScalarConstraint) -> Text
              readBoundEach (i, cs) = T.intercalate "\n" 
                [ "sc_lower_bound[" <> tt i <> "] = " <> d2s (constraintLowerBound cs) <> ";",
                  "sc_upper_bound[" <> tt i <> "] = " <> d2s (constraintUpperBound cs) <> ";"
                ] 
          readValCode (name, nId) 
            | Just val <- Map.lookup name valMap = generateReadValuesCode (name, product shape) ("ptr + " ++ show offset) val
            | otherwise = renderTemplate ([
                  ("name",  tt name),
                  ("size",  tt $ product shape),
                  ("offset",  tt offset)
                  ]) randomizeValueTemplate
            where
              offset = addressReal nId
              shape = retrieveShape nId expressionMap
          readValues = T.intercalate "\n" $ map readValCode varsAndParams
          writeResultCode :: Variable -> Text
          writeResultCode var
            | output == OutputHDF5 = renderTemplate ([
                  ("name",  tt $ varName var),
                  ("filePath",  tt $ varName var <> "_out.h5"),
                  ("address",  "ptr + " <> (tt . addressReal . nodeId $ var) ),
                  ("shapeLength",  tt . length . getShape . nodeId $ var),
                  ("shape",  T.intercalate ", " $ map tt . getShape . nodeId $ var)
                ]) writeHDF5Template 
            | output == OutputText = renderTemplate ([
                  ("name",  tt $ varName var),
                  ("filePath",  tt $ varName var <> "_out.txt"),
                  ("address",  "ptr + " <> (tt . addressReal . nodeId $ var) ),
                  ("size",  tt . product . getShape . nodeId $ var)
                ])
                writeTXTTemplate
          writeResult = T.intercalate "\n" $ map writeResultCode variables
            -- evaluatingCodes = undefined
        -- ["void evaluate_partial_derivatives_and_objective()"]
        --   ++ scoped (evaluating codegen $ objectiveId : map partialDerivativeId variables)
      -------------------------------------------------------------------------------
      -- evaluateObjectiveCodes = undefined
        -- ["void evaluate_objective()"]
        --   ++ scoped (evaluating codegen [objectiveId])
      -------------------------------------------------------------------------------
      -- evaluatePartialDerivativesCodes = undefined
        -- ["void evaluate_partial_derivatives()"]
        --   ++ scoped (evaluating codegen (map partialDerivativeId variables))
      -------------------------------------------------------------------------------
      -- evaluateScalarConstraintsCodes = undefined
        -- ["void evaluate_scalar_constraints()"]
        --   ++ scoped (evaluating codegen (map constraintValueId scalarConstraints))
      -------------------------------------------------------------------------------
      -- evaluateScalarConstraintsJacobianCodes = undefined
        -- ["void evaluate_scalar_constraints_jacobian()"]
        --   ++ scoped (evaluating codegen (concatMap constraintPartialDerivatives scalarConstraints))
      let codes = renderTemplate ([
                    ("fftUtils", if containsFTNode cExpressionMap then tt $ fftUtils else ""),
                    ("numHigherOrderVariables", tt numHigherOrderVariables),
                    ("numActualVariables", tt numActualVariables),
                    ("totalDoubles", tt totalReal),
                    ("totalComplexes", tt totalComplex),
                    ("varStartOffset", tt varStartOffset),
                    ("maxNumIterations", tt $ fromMaybe 0 maxIteration),
                    ("numScalarConstraints", tt numScalarConstraints),
                    ("varNames",  T.intercalate ", " $ map ttq varNames),
                    ("varSizes",  T.intercalate ", " $ map tt varSizes),
                    ("varOffsets",  T.intercalate ", " $ map tt varOffsets),
                    ("partialDerivativeOffsets",  T.intercalate ", "$ map tt partialDerivativeOffsets),
                    ("objectiveOffset", tt objectiveOffset),
                    ("scalarConstraintOffsets",  T.intercalate ", " $ map tt scalarConstraintOffsets),
                    ("scalarConstraintPartialDerivativeOffsets",  T.intercalate ", " $ 
                          map (\xs -> "{" <> (T.intercalate ", " $ map tt xs) <> "}") scalarConstraintPartialDerivativeOffsets),
                    ("readBounds",  readBounds),
                    ("readBoundScalarConstraints", tt readBoundScalarConstraints),
                    ("readValues",  readValues),
                    ("writeResult",  writeResult),
                    ("evaluatePartialDerivativesAndObjective",  evaluating codegen $ objectiveId : map partialDerivativeId variables),
                    ("evaluateObjective",  evaluating codegen $ [objectiveId]),
                    ("evaluatePartialDerivatives",  evaluating codegen (map partialDerivativeId variables)),
                    ("evaluateScalarConstraints",  evaluating codegen (map constraintValueId scalarConstraints)),
                    ("evaluateScalarConstraintsJacobian",  evaluating codegen (concatMap constraintPartialDerivatives scalarConstraints))
                    ]) cSimpleTemplate
            -- concat
            --   [ defineStuffs,
            --     constraintCodes,
            --     readValsCodes,
            --     writeResultCodes,
            --     evaluatingCodes,
            --     evaluateObjectiveCodes,
            --     evaluatePartialDerivativesCodes,
            --     evaluateScalarConstraintsCodes,
            --     evaluateScalarConstraintsJacobianCodes
            --   ]
      TIO.writeFile (folder </> "problem.c") codes
    where
      addressReal nID = let AddressReal res = cAddress nID in res
      -- params
      params :: [String]
      params = map fst $ paramsWithNodeID expressionMap
      -- value nodes
      varsAndParams :: [(String, NodeID)]
      varsAndParams = sortOn fst $ varsWithNodeID expressionMap ++ paramsWithNodeID expressionMap
      -------------------------------------------------------------------------------
      checkError :: Maybe String
      checkError
        | Just name <- find (not . (`Map.member` valMap)) params = Just $ "No value provided for " ++ name
        | otherwise,
          let isOk (var, nId)
                | Just val <- Map.lookup var valMap = compatible (retrieveShape nId expressionMap) val
                | otherwise = True,
          Just (name, shape) <- find (not . isOk) varsAndParams =
          Just $ name ++ "is of shape " ++ show shape ++ " but the value provided is not"
        | otherwise = Nothing
      -------------------------------------------------------------------------------
      codegen@CSimpleCodegen {..} = initCodegen cf expressionMap (map nodeId variables)
      variableOffsets = map (addressReal . nodeId) variables
      partialDerivativeOffsets = map (addressReal . partialDerivativeId) variables
      objectiveOffset = addressReal objectiveId
      -- For both variables and values
      readValCodeEach (name, nId) = undefined
        -- | Just val <- Map.lookup name valMap = generateReadValuesCode (name, product shape) ("ptr + " ++ show offset) val
        -- | otherwise =
        --   Scoped
        --     [ Printf ["Init value for " <> tt name <> " is not provided, generate random init for " <> tt name <> " ...\\n"],
        --       for "i" (product shape) $
        --         [("ptr[" <> tt offset <> "+ i]") := "0.2 * (double) rand() / RAND_MAX"]
        --     ]
        -- where
        --   offset = addressReal nId
        --   shape = retrieveShape nId expressionMap
      -------------------------------------------------------------------------------

        -- | output == OutputHDF5 =
        --   Scoped
        --     [ Printf ["Writing " <> tt name <> " to " <> tt name <> "_out.h5...\\n"],
        --       Statement "hid_t file, space, dset",
        --       ("hsize_t dims[" <> tt (length shape) <> "]") := ("{" <> T.intercalate ", " (map tt shape) <> "}"),
        --       "file" := (fun "H5Fcreate" [ttq $ name <> "_out.h5", "H5F_ACC_TRUNC", "H5P_DEFAULT", "H5P_DEFAULT"]),
        --       "space" := (fun "H5Screate_simple" [tt $ length shape, "dims", "NULL"]),
        --       "dset" := (fun "H5Dcreate" ["file", ttq name, "H5T_IEEE_F64LE", "space", "H5P_DEFAULT", "H5P_DEFAULT", "H5P_DEFAULT"]),
        --       Statement (fun "H5Dwrite" ["dset", "H5T_NATIVE_DOUBLE", "H5S_ALL", "H5S_ALL", "H5P_DEFAULT", "ptr + " <> tt offset]),
        --       Statement "H5Dclose(dset)",
        --       Statement "H5Sclose(space)",
        --       Statement "H5Fclose(file)"
        --     ]
        -- | output == OutputText =
        --   Scoped $
        --     [ Printf ["Writing " <> tt name <> " to " <> tt name <> "_out.txt...\\n"],
        --       Statement "FILE *file",
        --       "file" := (fun "fopen" [ttq $ name <> "_out.txt", ttq "w"]),
        --       for "i" (product shape) $
        --         [ Statement (fun "fprintf" ["file", ttq "%f", "ptr[" <> tt offset <> " + i]"]),
        --           if_ ("i + 1 < " <> tt (product shape)) $
        --             [ Statement (fun "fprintf" ["file", ttq " "])
        --             ]
        --         ],
        --       Statement "fclose(file)"
        --     ]
        -- where
        --   nId = nodeId variable
        --   name = varName variable
        --   offset = addressReal nId
        --   shape = retrieveShape nId expressionMap
      -------------------------------------------------------------------------------
      -- defineStuffs :: Code
      -- defineStuffs =
      --   [ "#include <math.h>",
      --     "#include <stdio.h>",
      --     "#include <stdlib.h>",
      --     "#include <time.h>",
      --     "#include \"hdf5.h\"",
      --     "#include <complex.h>",
      --     if containsFTNode expressionMap
      --       then tt fftUtils
      --       else "",
      --     "// number of (higher dimensional) variables ",
      --     "#define NUM_VARIABLES " <> tt (length variables),
      --     "// number of scalar variables (because each higher dimensional var is a grid of scalar variables)",
      --     "#define NUM_ACTUAL_VARIABLES " <> tt (sum variableSizes),
      --     "#define MEMORY_NUM_DOUBLES " <> tt totalReal,
      --     "#define MEMORY_NUM_COMPLEX_DOUBLES " <> tt totalComplex,
      --     "// all the actual double variables are allocated",
      --     "// one after another, starts from here",
      --     "#define VARS_START_OFFSET " <> tt (addressReal (nodeId . head $ variables)),
      --     "#define MAX_NUM_ITERATIONS " <> tt (fromMaybe 0 maxIteration),
      --     "const char* var_name[NUM_VARIABLES] = {" <> (T.intercalate ", " . map (ttq . varName) $ variables) <> "};",
      --     "const int var_size[NUM_VARIABLES] = {" <> (T.intercalate ", " . map tt $ variableSizes) <> "};",
      --     "const int var_offset[NUM_VARIABLES] = {" <> (T.intercalate ", " . map tt $ variableOffsets) <> "};",
      --     "const int partial_derivative_offset[NUM_VARIABLES] = {" <> (T.intercalate ", " . map tt $ partialDerivativeOffsets) <> "};",
      --     "const int objective_offset = " <> tt objectiveOffset <> ";",
      --     "double ptr[" <> tt totalReal <> "];",
      --     "complex double ptr_c[" <> tt totalComplex <> "];"
      --   ]
      -------------------------------------------------------------------------------
      -- constraintCodes =
      --   let varPosition = take (length variableSizes) $ scanl (+) 0 variableSizes
      --       varWithPos = zip vars varPosition
      --       getPos name = snd . fromMaybe (error "get starting position variable") . find ((== name) . fst) $ varWithPos
      --       readUpperBoundCode name val =
      --           generateReadValuesCode
      --             ((name ++ "_ub"), product $ variableShape name)
      --             ("upper_bound + " ++ show (getPos name))
      --             val
      --       readLowerBoundCode name val =
      --           generateReadValuesCode
      --             ((name ++ "_lb"), (product $ variableShape name))
      --             ("lower_bound + " ++ show (getPos name))
      --             val
      --       readBounds =
      --         let readBoundCodeEach cnt =
      --               case cnt of
      --                 BoxUpper name val -> readUpperBoundCode name val
      --                 BoxLower name val -> readLowerBoundCode name val
      --                 BoxBetween name (val1, val2) -> readLowerBoundCode name val1 ++ readUpperBoundCode name val2
      --          in concatMap readBoundCodeEach boxConstraints
      --       scalarConstraintDefineStuffs =
      --         [ "#define NUM_SCALAR_CONSTRAINT " <> tt (length scalarConstraints),
      --           "double sc_lower_bound[NUM_SCALAR_CONSTRAINT];",
      --           "double sc_upper_bound[NUM_SCALAR_CONSTRAINT];",
      --           "const int sc_offset[NUM_SCALAR_CONSTRAINT] = {"
      --             <> (T.intercalate "," . map (tt . addressReal . constraintValueId) $ scalarConstraints)
      --             <> "};",
      --           "",
      --           "const int sc_partial_derivative_offset[NUM_SCALAR_CONSTRAINT][NUM_VARIABLES] = {"
      --             <> T.intercalate
      --               ", "
      --               [ "{" <> T.intercalate "," (map (tt . addressReal) . constraintPartialDerivatives $ sc) <> "}"
      --                 | sc <- scalarConstraints
      --               ]
      --             <> "};"
      --         ]
      --       readBoundScalarConstraints =
      --         [ "sc_lower_bound[" <> tt i <> "] = " <> d2s val <> ";"
      --           | (i, val) <- zip [0 :: Int ..] $ map constraintLowerBound scalarConstraints
      --         ]
      --           <> [ "sc_upper_bound[" <> tt i <> "] = " <> d2s val <> ";"
      --                | (i, val) <- zip [0 :: Int ..] $ map constraintUpperBound scalarConstraints
      --              ]
      --    in [ "const int bound_pos[NUM_VARIABLES] = {" <> (T.intercalate ", " . map tt $ varPosition) <> "};",
      --         "double lower_bound[NUM_ACTUAL_VARIABLES];",
      --         "double upper_bound[NUM_ACTUAL_VARIABLES];"
      --       ]
      --         ++ scalarConstraintDefineStuffs
      --         ++ [ "void read_bounds() {", --
      --              "  for (int i = 0; i < NUM_ACTUAL_VARIABLES; i++) {",
      --              "    lower_bound[i] = -INFINITY;",
      --              "    upper_bound[i] = INFINITY;",
      --              "  }"
      --            ]
      --         ++ indent 2 readBounds
      --         ++ indent 2 readBoundScalarConstraints --
      --         ++ ["}"]
      -------------------------------------------------------------------------------
      readValsCodes = undefined
        -- ["void read_values() {"]
        --   ++ ["  srand(time(NULL));"] --
        --   ++ scoped (concatMap (fromCCode . readValCodeEach) varsAndParams)
        --   ++ ["}"] --
          -------------------------------------------------------------------------------
      writeResultCodes = undefined
        -- ["void write_result()"]
        --   ++ scoped (concatMap (fromCCode . writeResultCodeEach) variables)
      -------------------------------------------------------------------------------
      evaluatingCodes = undefined
        -- ["void evaluate_partial_derivatives_and_objective()"]
        --   ++ scoped (evaluating codegen $ objectiveId : map partialDerivativeId variables)
      -------------------------------------------------------------------------------
      evaluateObjectiveCodes = undefined
        -- ["void evaluate_objective()"]
        --   ++ scoped (evaluating codegen [objectiveId])
      -------------------------------------------------------------------------------
      evaluatePartialDerivativesCodes = undefined
        -- ["void evaluate_partial_derivatives()"]
        --   ++ scoped (evaluating codegen (map partialDerivativeId variables))
      -------------------------------------------------------------------------------
      evaluateScalarConstraintsCodes = undefined
        -- ["void evaluate_scalar_constraints()"]
        --   ++ scoped (evaluating codegen (map constraintValueId scalarConstraints))
      -------------------------------------------------------------------------------
      evaluateScalarConstraintsJacobianCodes = undefined
        -- ["void evaluate_scalar_constraints_jacobian()"]
        --   ++ scoped (evaluating codegen (concatMap constraintPartialDerivatives scalarConstraints))

-------------------------------------------------------------------------------

toShapeString :: Shape -> T.Text
toShapeString shape
  | length shape < 3 =
    "{"
      <> (T.intercalate ", " . map tt $ shape <> replicate (3 - length shape) 1)
      <> "}"
  | otherwise = "{" <> (T.intercalate ", " . map tt $ shape) <> "}"

-------------------------------------------------------------------------------

generateReadValuesCode :: (String, Int) -> String -> Val -> Text
generateReadValuesCode (name, size) address val = 
  case val of
    VScalar value -> codeCToText $ Scoped [("*(" <> tt address <> ")") := (tt value)]
    V1D _ -> readFileText (tt name <> ".txt")
    V2D _ -> readFileText (tt name <> ".txt")
    V3D _ -> readFileText (tt name <> ".txt")
    VFile (TXT filePath) -> readFileText $ tt filePath
    VFile (HDF5 filePath dataset) -> readFileHD5 (tt filePath) (tt dataset)
    VNum value ->
      codeCToText $ for "i" size $
        [ ("*(" <> tt address <> " + i)") := (tt value)
        ]
  where
    readFileText filePath = renderTemplate ([
        ("name",  tt name),
        ("filePath",  filePath),
        ("size",  tt size),
        ("address",  tt address)
      ]) readTXTTemplate
    
  --     -- Scoped
  --     --   [ Printf ["Reading " <> tt name <> " from text file " <> filePath <> " ...\\n"],
  --     --     "FILE *fp" := (fun "fopen" [ttq filePath, ttq "r"]),
  --     --     for "i" size $
  --     --       [ Statement (fun "fscanf" ["fp", ttq "%lf", tt address <> " + i"])
  --     --       ],
  --     --     Statement (fun "fclose" ["fp"])
  --     --   ]
    readFileHD5 filePath dataset =
        renderTemplate ([
          ("name",  tt name),
          ("filePath",  filePath),
          ("size",  tt size),
          ("address",  tt address),
          ("dataset",  dataset)
        ]) readHDF5Template
  --     Scoped
  --       [ Printf ["Reading " <> tt name <> " from HDF5 file in dataset " <> dataset <> " from " <> filePath <> " ...\\n"],
  --         Statement "hid_t file, dset",
  --         "file" := (fun "H5Fopen" [ttq filePath, "H5F_ACC_RDONLY", "H5P_DEFAULT"]),
  --         "dset" := (fun "H5Dopen" ["file", ttq dataset, "H5P_DEFAULT"]),
  --         Statement (fun "H5Dread" ["dset", "H5T_NATIVE_DOUBLE", "H5S_ALL", "H5S_ALL", "H5P_DEFAULT", tt address]),
  --         Statement "H5Fclose (file)",
  --         Statement "H5Dclose (dset)"
  --       ]
