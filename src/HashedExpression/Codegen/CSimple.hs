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
import Data.List (find, foldl', partition, sortOn, tails)
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
import HashedExpression.Embed.FFTW (fftUtils)
import HashedExpression.Internal (topologicalSortManyRoots, unwrap)
import HashedExpression.Internal.Expression (DimensionType, ET (..), Expression, ExpressionMap, NumType, Op (..), Shape, exMap)
import HashedExpression.Internal.Node (retrieveElementType, retrieveNode, retrieveOp, retrieveShape)
import HashedExpression.Internal.Utils
import HashedExpression.Problem
import HashedExpression.Value
import System.FilePath
import Prelude hiding ((!!))

-------------------------------------------------------------------------------

-- | Generate simple C code
data CSimpleConfig = CSimpleConfig

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
    reAt :: NodeID -> Index -> Text
  }

-------------------------------------------------------------------------------

d2s :: Double -> Text
d2s val
  | val == ninf = "-INFINITY"
  | val == inf = "INFINITY"
  | otherwise = showT val

-- | Helpers for code generation
scoped :: Code -> Code
scoped codes = ["{"] ++ indent 2 codes ++ ["}"]

-- | Helper functions to generate codes
for :: Text -> Int -> Code -> Code
for iter bound codes =
  scoped $
    [ [i|int #{iter};|],
      [i|for (#{iter} = 0; #{iter} < #{bound}; #{iter}++)|]
    ]
      ++ scoped codes

if_ :: Text -> Code -> Code
if_ condition codes = [[i|if (#{condition})|]] ++ scoped codes

elseif :: Text -> Code -> Code
elseif condition codes = [[i|else if (#{condition})|]] ++ scoped codes

else_ :: Code -> Code
else_ codes = ["else"] ++ scoped codes

initCodegen :: CSimpleConfig -> ExpressionMap -> [NodeID] -> CSimpleCodegen
initCodegen _ mp consecutiveIDs =
  CSimpleCodegen
    { cExpressionMap = mp,
      cAddress = addressMap,
      cMemSize = totalSize,
      (!!) = access,
      imAt = imAt,
      reAt = reAt
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
    reAt = access
    imAt nID offset = access nID $ offset <> " + " <> showT (product (retrieveShape nID mp))

-------------------------------------------------------------------------------
evaluating :: CSimpleCodegen -> [Int] -> Code
evaluating CSimpleCodegen {..} rootIDs =
  concatMap genCode $ topologicalSortManyRoots (cExpressionMap, rootIDs)
  where
    shapeOf nID = retrieveShape nID cExpressionMap
    elementTypeOf nID = retrieveElementType nID cExpressionMap
    addressOf :: Int -> Text
    addressOf nID = [I.i|(ptr + #{cAddress nID})|]
    [i, j, k, nooffset] = ["i", "j", "k", "0"]
    len nID = product (retrieveShape nID cExpressionMap)
    genCode :: Int -> Code
    genCode n =
      let (shape, _, op) = retrieveNode n cExpressionMap
       in case op of
            Var _ -> []
            Const val -> for i (len n) [[I.i|#{n !! i} = #{val};|]]
            Sum args ->
              let sumAt i = T.intercalate " + " $ map (!! i) args
               in for i (len n) [[I.i|#{n !! i} = #{sumAt i};|]]
            Mul args ->
              let prodAt i = T.intercalate " * " $ map (!! i) args
               in for i (len n) [[I.i|#{n !! i} = #{prodAt i};|]]
            Power x arg -> for i (len n) [[I.i|#{n !! i} = pow(#{arg !! i}, #{x});|]]
            Neg arg -> for i (len n) [[I.i|#{n !! i} = - #{arg !! i};|]]
            Scale scalar arg -> for i (len n) [[I.i|#{n !! i} = #{scalar !! nooffset} * #{arg !! i};|]]
            Div arg1 arg2 -> for i (len n) [[I.i|#{n !! i} = #{arg1 !! i} / #{arg2 !! i};|]]
            Sqrt arg -> for i (len n) [[I.i|#{n !! i} = sqrt(#{arg !! i});|]]
            Sin arg -> for i (len n) [[I.i|#{n !! i} = sin(#{arg !! i});|]]
            Cos arg -> for i (len n) [[I.i|#{n !! i} = cos(#{arg !! i});|]]
            Tan arg -> for i (len n) [[I.i|#{n !! i} = tan(#{arg !! i});|]]
            Exp arg -> for i (len n) [[I.i|#{n !! i} = exp(#{arg !! i});|]]
            Log arg -> for i (len n) [[I.i|#{n !! i} = log(#{arg !! i});|]]
            Sinh arg -> for i (len n) [[I.i|#{n !! i} = sinh(#{arg !! i});|]]
            Cosh arg -> for i (len n) [[I.i|#{n !! i} = cosh(#{arg !! i});|]]
            Tanh arg -> for i (len n) [[I.i|#{n !! i} = tanh(#{arg !! i});|]]
            Asin arg -> for i (len n) [[I.i|#{n !! i} = asin(#{arg !! i});|]]
            Acos arg -> for i (len n) [[I.i|#{n !! i} = acos(#{arg !! i});|]]
            Atan arg -> for i (len n) [[I.i|#{n !! i} = atan(#{arg !! i});|]]
            Asinh arg -> for i (len n) [[I.i|#{n !! i} = asinh(#{arg !! i});|]]
            Acosh arg -> for i (len n) [[I.i|#{n !! i} = acosh(#{arg !! i});|]]
            Atanh arg -> for i (len n) [[I.i|#{n !! i} = atanh(#{arg !! i});|]]
            RealImag arg1 arg2 ->
              for i (len n) [[I.i|#{n !! i} = #{arg1 !! i};|]]
                ++ for i (len n) [[I.i|#{n `imAt` i} = #{arg2 !! i};|]]
            InnerProd arg1 arg2
              | null (shapeOf arg1) -> [[I.i|#{n !! nooffset} = #{arg1 !! nooffset} * #{arg2 !! nooffset};|]]
              | otherwise ->
                let initCodes = [[I.i|double acc = 0;|]]
                    codes = for i (len arg1) [[I.i|acc += #{arg1 !! i} * #{arg2 !! i};|]]
                    afterCodes = [[I.i|#{n !! nooffset} = acc;|]]
                 in scoped $ initCodes ++ codes ++ afterCodes
            Piecewise marks condition branches ->
              let m : ms = map showT marks
                  Just (b : bs, lst) = viewR branches
                  elseifEach (m, b) =
                    elseif
                      [I.i|#{condition !! i} <= #{m}|]
                      [[I.i|#{n !! i} = #{b !! i};|]]
               in for i (len n) $
                    if_
                      [I.i|#{condition !! i} <= #{m}|]
                      [[I.i|#{n !! i} = #{b !! i};|]]
                      ++ concatMap elseifEach (zip ms bs)
                      ++ else_ [[I.i|#{n !! i} = #{lst !! i};|]]
            Rotate [amount] arg ->
              let [size] = shape
               in for i size $
                    [ [I.i|int ai = (#{i} - #{amount} + #{size}) % #{size};|],
                      [I.i|#{n !! i} = #{arg !! "ai"};|]
                    ]
            Rotate [amount1, amount2] arg ->
              let [size1, size2] = shape
                  toIndex i j = [I.i|#{i} * #{size2} + #{j}|]
               in for i size1 $
                    for j size2 $
                      [ [I.i|int ai = (#{i} - #{amount1} + #{size1}) % #{size1};|],
                        [I.i|int aj = (#{j} - #{amount2} + #{size2}) % #{size2};|],
                        [I.i|#{n !! (toIndex i j)} = #{arg !! (toIndex "ai" "aj")};|]
                      ]
            Rotate [amount1, amount2, amount3] arg ->
              let [size1, size2, size3] = shape
                  toIndex i j k = [I.i|#{i} * #{size2} * #{size3} + #{j} * #{size3} + #{k}|]
               in for i size1 $
                    for j size2 $
                      for k size3 $
                        [ [I.i|int ai = (#{i} - #{amount1} + #{size1}) % #{size1};|],
                          [I.i|int aj = (#{j} - #{amount2} + #{size2}) % #{size2};|],
                          [I.i|int ak = (#{k} - #{amount3} + #{size3}) % #{size3}'|],
                          [I.i|#{n !! (toIndex i j k)} = #{arg !! (toIndex "ai" "aj" "ak")};|]
                        ]
            TwiceReFT arg ->
              case shape of
                [size] -> [[I.i|re_dft_twice_1d(#{size}, #{addressOf arg}, #{addressOf n});|]]
                [size1, size2] -> [[I.i|re_dft_twice_2d(#{size1}, #{size2}, #{addressOf arg}, #{addressOf n});|]]
            TwiceImFT arg ->
              case shape of
                [size] -> [[I.i|im_dft_twice_1d(#{size}, #{addressOf arg}, #{addressOf n});|]]
                [size1, size2] -> [[I.i|im_dft_twice_2d(#{size1}, #{size2}, #{addressOf arg}, #{addressOf n});|]]
            ReFT arg ->
              case shape of
                [size] -> [[I.i|dft_1d(#{size}, #{addressOf arg}, #{addressOf n}, REAL);|]]
                [size1, size2] -> [[I.i|dft_2d(#{size1}, #{size2}, #{addressOf arg}, #{addressOf n}, REAL);|]]
            ImFT arg ->
              case shape of
                [size] -> [[I.i|dft_1d(#{size}, #{addressOf arg}, #{addressOf n}, IMAG);|]]
                [size1, size2] -> [[I.i|dft_2d(#{size1}, #{size2}, #{addressOf arg}, #{addressOf n}, IMAG);|]]
            node -> error $ "Other node type should not be here after normalized " ++ show node

-------------------------------------------------------------------------------
instance Codegen CSimpleConfig where
  generateProblemCode :: CSimpleConfig -> Problem -> ValMaps -> GenResult
  generateProblemCode config Problem {..} valMaps
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
                --                writeVarCodes,
                evaluatingCodes,
                evaluateObjectiveCodes,
                evaluatePartialDerivativesCodes,
                evaluateScalarConstraintsCodes,
                evaluateScalarConstraintsJacobianCodes
              ]
      TIO.writeFile (folder </> "problem.c") $ T.intercalate "\n" codes
    where
      -------------------------------------------------------------------------------
      -- variables we're trying to optimize over
      vars :: [String]
      vars = map varName variables
      -- check if this name is optimizing variable or fixed value
      isVariable :: String -> Bool
      isVariable v = v `elem` vars
      -- var nodes, can be variables or values
      vs :: [(String, Int)]
      vs = sortOn fst $ varNodesWithId expressionMap
      -- values node
      vals :: [String]
      vals = filter (not . isVariable) . map fst $ vs
      -- get shape of a variable
      variableShape :: String -> Shape
      variableShape name =
        let nId = case find ((== name) . varName) variables of
              Just var -> nodeId var
              _ -> error "not a variable but you're getting it's shape"
         in retrieveShape nId expressionMap
      variableShapes :: [Shape]
      variableShapes = map (variableShape . varName) variables
      -- size of each variable (product of it's shape)
      variableSizes :: [Int]
      variableSizes = map product variableShapes
      -------------------------------------------------------------------------------
      checkError :: Maybe String
      checkError
        | Just name <- find (not . (`Map.member` valMaps)) vals = Just $ "No value provided for " ++ name
        | otherwise,
          let isOk (var, nId)
                | Just val <- Map.lookup var valMaps = compatible (retrieveShape nId expressionMap) val
                | otherwise = True,
          Just (var, shape) <- find (not . isOk) vs =
          Just $ "variable " ++ var ++ "is of shape " ++ show shape ++ " but the value provided is not"
        | otherwise = Nothing
      -------------------------------------------------------------------------------
      codegen@CSimpleCodegen {..} = initCodegen config expressionMap (map nodeId variables)
      variableOffsets = map (cAddress . nodeId) variables
      partialDerivativeOffsets = map (cAddress . partialDerivativeId) variables
      objectiveOffset = cAddress objectiveId
      -- For both variables and values
      readValCodeEach (name, nId)
        | Just val <- Map.lookup name valMaps = generateReadValuesCode (name, product shape) ("ptr + " ++ show offset) val
        | otherwise =
          scoped
            [ [i|printf("Init value for #{name} is not provided, generating random for #{name} ... \\n");|],
              "int i;",
              [i|for (i = 0; i < #{product shape}; i++){|],
              [i|  ptr[#{offset} + i] = ((double) rand() / RAND_MAX);|],
              "}"
            ]
        where
          offset = cAddress nId
          shape = retrieveShape nId expressionMap
      -------------------------------------------------------------------------------
      --      writeVarCodeEach (name, nId) =
      --        [ [i|for (i = 0; i < #{product shape}; i++){|],
      --          [i|  fprintf(fp,"#{name} %d %f",i,ptr[#{offset} + i]);|],
      --          "}"
      --        ]
      --        where
      --          offset = cAddress nId
      --          shape = retrieveShape nId expressionMap
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
          "const int var_num_dim[NUM_VARIABLES] = {"
            <> (T.intercalate ", " . map (showT . length) $ variableShapes)
            <> "};",
          "const int var_shape[NUM_VARIABLES][3] = {"
            <> (T.intercalate ", " . map toShapeString $ variableShapes)
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
              generateReadValuesCode
                ((name ++ "_ub"), product $ variableShape name)
                ("upper_bound + " ++ show (getPos name))
                val
            readLowerBoundCode name val =
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
          ++ scoped (concatMap readValCodeEach vs)
          ++ ["}"] --
          -------------------------------------------------------------------------------
          --      writeVarCodes =
          --        ["void print_vars() {"]
          --          ++ [[i|  FILE *fp = fopen("solutions.out","w");|]]
          --          ++ scoped (["  int i;"] ++ concatMap writeVarCodeEach vs)
          --          ++ [ "  fclose(fp);",
          --               "}"
          --             ]
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

generateReadValuesCode :: (String, Int) -> String -> Val -> Code
generateReadValuesCode (name, size) address val =
  case val of
    VScalar value -> scoped ["*(" <> T.pack address <> ") = " <> showT value <> ";"]
    V1D _ -> readFileText (T.pack name <> ".txt")
    V2D _ -> readFileText (T.pack name <> ".txt")
    V3D _ -> readFileText (T.pack name <> ".txt")
    VFile (TXT filePath) -> readFileText $ T.pack filePath
    VFile (HDF5 filePath dataset) -> readFileHD5 (T.pack filePath) (T.pack dataset)
    VNum value ->
      scoped
        [ "int i;",
          [i|for (i = 0; i < #{size}; i++){|],
          [i|  *(#{address} + i) = #{value};|],
          "}"
        ]
  where
    readFileText filePath =
      scoped
        [ [i|printf("Reading #{name} from text file #{filePath} ... \\n");|],
          [i|FILE *fp = fopen("#{filePath}", "r");|],
          "int i;",
          [i|for (i = 0; i < #{size}; i++){|],
          [i|  fscanf(fp, "%lf", #{address} + i);|],
          "}",
          "fclose(fp);"
        ]
    readFileHD5 filePath dataset =
      scoped
        [ [i|printf("Reading #{name} from HDF5 file in dataset #{dataset} ... \\n");|],
          "hid_t file, dset;",
          [i|file = H5Fopen("#{filePath}", H5F_ACC_RDONLY, H5P_DEFAULT);|],
          [i|dset = H5Dopen(file, "#{dataset}", H5P_DEFAULT);|],
          [i|H5Dread (dset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, #{address});|],
          "H5Fclose (file);",
          "H5Dclose (dset);"
        ]
