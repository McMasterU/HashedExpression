module HashedExpression.Codegen.CSimple where

import Control.Monad (when)
import Data.Array ((!), indices)
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
import HashedExpression.Internal.Expression (DimensionType, ET (..), Expression, ExpressionMap, Node (..), NumType, Shape, exMap)
import HashedExpression.Internal.Inner (containsFTNode, topologicalSortManyRoots, unwrap, varNodesWithId)
import HashedExpression.Internal.Node (nodeElementType, retrieveElementType, retrieveInternal, retrieveNode, retrieveShape)
import HashedExpression.Internal.Utils
import HashedExpression.Problem
import Prelude hiding ((!!))

-------------------------------------------------------------------------------
ninf :: Double
ninf = -1 / 0

inf :: Double
inf = 1 / 0

d2s :: Double -> Text
d2s val
  | val == ninf = "-INFINITY"
  | val == inf = "INFINITY"
  | otherwise = showT val

-- | Generate simple C code
data CSimpleConfig = CSimpleConfig

type Address = Int

type NodeID = Int

data CSimpleCodegen
  = CSimpleCodegen
      { cExpressionMap :: ExpressionMap,
        cAddress :: NodeID -> Address,
        cMemSize :: Int,
        (!!) :: NodeID -> Text -> Text,
        imAt :: NodeID -> Text -> Text
      }

-------------------------------------------------------------------------------

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

-- | Compute the inner offset:
-- e.g localOffset [3, 4, 5] [2, 0, 1] = 2 * (4 * 5) + 0 * (5) + 1
localOffset :: [Int] -> [Int] -> Int
localOffset shape indices
  | length shape == length indices =
    sum . zipWith (*) indices . map product . tail . tails $ shape
  | otherwise = error $ "shape and indices are not compatible" ++ show shape ++ show indices

-------------------------------------------------------------------------------
instance Codegen CSimpleCodegen CSimpleConfig where
  initCodegen :: CodegenInit -> CSimpleConfig -> CSimpleCodegen
  initCodegen (CodegenInit mp consecutiveIDs) _ =
    CSimpleCodegen
      { cExpressionMap = mp,
        cAddress = addressMap,
        cMemSize = totalSize,
        (!!) = access,
        imAt = imAt
      }
    where
      (cs, rest) = partition (`Set.member` Set.fromList consecutiveIDs) (IM.keys mp)
      f (addressMap, curSize) nID =
        let (shape, node) = retrieveInternal nID mp
            et = nodeElementType node mp
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
        let (shape, node) = retrieveInternal n cExpressionMap
         in case node of
              Var _ -> []
              Const val -> for i (len n) [[I.i|#{n !! i} = #{val};|]]
              Sum R args ->
                let sumAt i = T.intercalate " + " $ map (!! i) args
                 in for i (len n) [[I.i|#{n !! i} = #{sumAt i};|]]
              Mul R args ->
                let prodAt i = T.intercalate " * " $ map (!! i) args
                 in for i (len n) [[I.i|#{n !! i} = #{prodAt i};|]]
              Power x arg -> for i (len n) [[I.i|#{n !! i} = pow(#{arg !! i}, #{x});|]]
              Neg R arg -> for i (len n) [[I.i|#{n !! i} = - #{arg !! i};|]]
              Scale R scalar arg -> for i (len n) [[I.i|#{n !! i} = #{scalar !! nooffset} * #{arg !! i};|]]
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
              InnerProd R arg1 arg2
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
                 in for i size1 $ for j size2 $
                      [ [I.i|int ai = (#{i} - #{amount1} + #{size1}) % #{size1};|],
                        [I.i|int aj = (#{j} - #{amount2} + #{size2}) % #{size2};|],
                        [I.i|#{n !! (toIndex i j)} = #{arg !! (toIndex "ai" "aj")};|]
                      ]
              Rotate [amount1, amount2, amount3] arg ->
                let [size1, size2, size3] = shape
                    toIndex i j k = [I.i|#{i} * #{size2} * #{size3} + #{j} * #{size3} + #{k}|]
                 in for i size1 $ for j size2 $ for k size3 $
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
  generateProblemCode :: CSimpleConfig -> Problem -> ValMaps -> GenResult
  generateProblemCode config Problem {..} valMaps
    | Just errorMsg <- checkError = Invalid errorMsg
    | otherwise =
      Success $ \folder -> do
        let codes =
              defineStuffs
                ++ constraintCodes
                ++ readVals
                ++ evaluatingCodes
                ++ evaluateObjectiveCodes
                ++ evaluatePartialDerivatives
                ++ evaluateScalarConstraints
                ++ evaluateScalarConstraintsJacobian
        let writeVal (var, val) =
              when (valueFromHaskell val) $ do
                let str = T.unwords . map showT . valElems $ val
                TIO.writeFile (folder ++ "/" ++ var ++ ".txt") str
        let writeUpperBound var val =
              when (valueFromHaskell val) $ do
                let str = T.unwords . map showT . valElems $ val
                    fileName = var ++ "_ub.txt"
                TIO.writeFile (folder ++ "/" ++ fileName) str
        let writeLowerBound var val =
              when (valueFromHaskell val) $ do
                let str = T.unwords . map showT . valElems $ val
                    fileName = var ++ "_lb.txt"
                TIO.writeFile (folder ++ "/" ++ fileName) str
        mapM_ writeVal $ Map.toList valMaps
        let writeEach cnt =
              case cnt of
                BoxLower var val -> writeLowerBound var val
                BoxUpper var val -> writeUpperBound var val
                BoxBetween var (val1, val2) ->
                  writeLowerBound var val1 >> writeUpperBound var val2
        mapM_ writeEach boxConstraints
        TIO.writeFile (folder ++ "/problem.c") $ T.intercalate "\n" codes
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
      -- Check if is variable or a fixed value
      checkError :: Maybe String
      checkError
        | Just name <- find (not . (`Map.member` valMaps)) vals = Just $ "No value provided for " ++ name
        | otherwise,
          let isOk (var, nId)
                | Just val <- Map.lookup var valMaps =
                  compatible (retrieveShape nId expressionMap) val
                | otherwise = True,
          Just (var, shape) <- find (not . isOk) vs =
          Just $ "variable " ++ var ++ "is of shape " ++ show shape ++ " but the value provided is not"
        | otherwise = Nothing
      -------------------------------------------------------------------------------
      codegen@CSimpleCodegen {..} = initCodegen (CodegenInit expressionMap (map nodeId variables)) config
      variableOffsets = map (cAddress . nodeId) variables
      partialDerivativeOffsets = map (cAddress . partialDerivativeId) variables
      objectiveOffset = cAddress objectiveId
      -- For both variables and values
      readValCodeEach (name, nId)
        | Just val <- Map.lookup name valMaps = generateReadValuesCode name val ("ptr + " ++ show offset) (product shape)
        | otherwise =
          scoped
            [ "printf(\"Init value for " <> T.pack name <> " is not provided, generating random for " <> T.pack name <> "....\\n\");",
              "int i;",
              "for (i = 0; i < " <> showT offset <> "; i<>) {",
              "  ptr[" <> showT offset <> " + i] = ((double) rand() / (RAND_MAX))",
              "}"
            ]
        where
          offset = cAddress nId
          shape = retrieveShape nId expressionMap
      objectiveAndGradient = objectiveId : map partialDerivativeId variables
      -- MARK: codes part --
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
          "",
          "",
          "#define NUM_VARIABLES " <> showT (length variables),
          "#define NUM_ACTUAL_VARIABLES " <> showT (sum variableSizes),
          "#define MEM_SIZE " <> showT cMemSize,
          "",
          "// all the actual double variables are allocated",
          "// one after another, starts from here",
          "#define VARS_START_OFFSET " <> showT (cAddress (nodeId . head $ variables)),
          "",
          "",
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
          "double ptr[MEM_SIZE];",
          "",
          ""
        ]
      constraintCodes =
        let varPosition =
              take (length variableSizes) $ scanl (+) 0 variableSizes
            varWithPos = zip vars varPosition
            getPos name =
              snd . fromMaybe (error "get starting position variable")
                . find ((== name) . fst)
                $ varWithPos
            readUpperBoundCode name val =
              generateReadValuesCode
                (name ++ "_ub")
                val
                ("upper_bound + " ++ show (getPos name))
                (product $ variableShape name)
            readLowerBoundCode name val =
              generateReadValuesCode
                (name ++ "_lb")
                val
                ("lower_bound + " ++ show (getPos name))
                (product $ variableShape name)
            readBounds =
              let readBoundCodeEach cnt =
                    case cnt of
                      BoxUpper name val -> readUpperBoundCode name val
                      BoxLower name val -> readLowerBoundCode name val
                      BoxBetween name (val1, val2) ->
                        readLowerBoundCode name val1
                          ++ readUpperBoundCode name val2
               in concatMap readBoundCodeEach boxConstraints
            scalarConstraintDefineStuffs =
              [ "#define NUM_SCALAR_CONSTRAINT " <> showT (length scalarConstraints),
                "",
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
                  <> "};",
                "",
                ""
              ]
            readBoundScalarConstraints =
              [ "sc_lower_bound[" <> showT i <> "] = " <> d2s val <> ";"
                | (i, val) <-
                    zip [0 ..] $ map constraintLowerBound scalarConstraints
              ]
                <> [ "sc_upper_bound[" <> showT i <> "] = " <> d2s val <> ";"
                     | (i, val) <-
                         zip [0 ..] $ map constraintUpperBound scalarConstraints
                   ]
         in [ "const int bound_pos[NUM_VARIABLES] = {"
                <> (T.intercalate ", " . map showT $ varPosition)
                <> "};",
              "double lower_bound[NUM_ACTUAL_VARIABLES];",
              "double upper_bound[NUM_ACTUAL_VARIABLES];",
              "",
              ""
            ]
              ++ scalarConstraintDefineStuffs
              ++ [ "void read_bounds() {", --
                   "  for (int i = 0; i < NUM_ACTUAL_VARIABLES; i++) {",
                   "    lower_bound[i] = -INFINITY;",
                   "    upper_bound[i] = INFINITY;",
                   "  }"
                 ]
              ++ scoped readBounds
              ++ scoped readBoundScalarConstraints --
              ++ ["}"] --
      readVals =
        ["void read_values() {"]
          ++ ["  srand(time(NULL));"] --
          ++ scoped (concatMap readValCodeEach vs)
          ++ ["}"] --
      evaluatingCodes =
        ["void evaluate_partial_derivatives_and_objective()"]
          ++ scoped (evaluating codegen objectiveAndGradient)
      evaluateObjectiveCodes =
        ["void evaluate_objective()"]
          ++ scoped (evaluating codegen [objectiveId])
      evaluatePartialDerivatives =
        ["void evaluate_partial_derivatives()"]
          ++ scoped (evaluating codegen (map partialDerivativeId variables))
      evaluateScalarConstraints =
        ["void evaluate_scalar_constraints()"]
          ++ scoped (evaluating codegen (map constraintValueId scalarConstraints))
      evaluateScalarConstraintsJacobian =
        ["void evaluate_scalar_constraints_jacobian()"]
          ++ scoped (evaluating codegen (concatMap constraintPartialDerivatives scalarConstraints))

toShapeString :: Shape -> T.Text
toShapeString shape
  | length shape < 3 =
    "{"
      <> (T.intercalate ", " . map showT $ shape <> replicate (3 - length shape) 1)
      <> "}"
  | otherwise = "{" <> (T.intercalate ", " . map showT $ shape) <> "}"

-------------------------------------------------------------------------------

-- | Generate a fully working C program that compute the expression and
-- print out the result, mostly used for testing
singleExpressionCProgram ::
  (DimensionType d, NumType et) => ValMaps -> Expression d et -> Code
singleExpressionCProgram valMaps expr =
  [ "#include <math.h>", --
    "#include <stdio.h>",
    "#include <stdlib.h>",
    if containsFTNode $ exMap expr
      then T.pack $ fftUtils
      else "",
    "int main()" --
  ]
    ++ scoped (initMemory ++ assignVals ++ codes ++ printValue ++ releaseMemory)
  where
    (mp, n) = unwrap expr
    bound = product (retrieveShape n mp)
    et = retrieveElementType n mp
    codeGen = initCodegen (CodegenInit mp []) CSimpleConfig
    [i, j, k, nooffset] = ["i", "j", "k", "0"]
    initMemory = [[I.i|double *ptr = malloc(sizeof(double) * #{cMemSize codeGen});|]]
    -- assign value to variables
    assignVals = assigningValues codeGen valMaps
    -- codes to compute
    codes = evaluating codeGen [n]
    -- print the value of expression
    printValue
      | et == R = for i bound [[I.i|printf("%f ", #{(!!) codeGen n i});|]]
      | et == C =
        for i bound [[I.i|printf("%f ", #{(!!) codeGen n i});|]]
          ++ [[I.i|printf("\\n");|]]
          ++ for i bound [[I.i|printf("%f ", #{imAt codeGen n i});|]]
    releaseMemory = ["free(ptr);"]
    -------------------------------------------------------------------------------
    assigningValues :: CSimpleCodegen -> ValMaps -> Code
    assigningValues CSimpleCodegen {..} valMaps = concatMap codesForVar vars
      where
        [i, j, k, nooffset] = ["i", "j", "k", "0"]
        vars :: [(Int, String)]
        vars =
          let toVar nId
                | Var varName <- retrieveNode nId cExpressionMap = Just (nId, varName)
                | otherwise = Nothing
           in mapMaybe toVar . IM.keys $ cExpressionMap
        codesForVar :: (Int, String) -> Code
        codesForVar (n, varName) =
          case Map.lookup varName valMaps of
            Just (VScalar val) -> [[I.i|#{n !! nooffset} = #{val};|]]
            Just (V1D array1d) ->
              let assignIndex id = [[I.i|#{n !! (showT id)} = #{array1d ! id};|]]
               in concatMap assignIndex $ indices array1d
            Just (V2D array2d) ->
              let shape = retrieveShape n cExpressionMap
                  assignIndex (id1, id2) =
                    [[I.i|#{n !! showT (localOffset shape [id1, id2])} = #{array2d ! (id1, id2)};|]]
               in concatMap assignIndex $ indices array2d
            Just (V3D array3d) ->
              let shape = retrieveShape n cExpressionMap
                  assignIndex (id1, id2, id3) =
                    [[I.i|#{n !! showT (localOffset shape [id1, id2, id3])} = #{array3d ! (id1, id2, id2)};|]]
               in concatMap assignIndex $ indices array3d
            _ -> []

generateReadValuesCode :: String -> Val -> String -> Int -> Code
generateReadValuesCode name val address numDoubles =
  case val of
    VScalar value -> scoped ["*(" <> T.pack address <> ") = " <> showT value]
    V1D _ -> readFileText (T.pack name <> ".txt")
    V2D _ -> readFileText (T.pack name <> ".txt")
    V3D _ -> readFileText (T.pack name <> ".txt")
    VFile (TXT filePath) -> readFileText $ T.pack filePath
    VFile (HDF5 filePath dataset) -> readFileHD5 (T.pack filePath) (T.pack dataset)
    VNum value ->
      scoped
        [ "int i;",
          "for (i = 0; i < " <> showT numDoubles <> "; i<>) { ",
          "  *(" <> T.pack address <> " + i) = " <> showT value <> ";",
          "}"
        ]
  where
    readFileText filePath =
      scoped
        [ "printf(\"Reading " <> T.pack name <> " from text file " <> filePath <> "....\\n\");",
          "FILE *fp = fopen(\"" <> filePath <> "\", \"r\");",
          "int i;",
          "for (i = 0; i < " <> showT numDoubles <> "; i<>) { ",
          "  fscanf(fp, \"%lf\", " <> T.pack address <> " + i);",
          "}",
          "fclose(fp);"
        ]
    readFileHD5 filePath dataset =
      scoped
        [ "printf(\"Reading " <> T.pack name <> " from HDF5 file " <> filePath <> " in dataset " <> dataset <> "....\\n\");",
          "hid_t file, dset;",
          "file = H5Fopen (\"" <> filePath <> "\", H5F_ACC_RDONLY, H5P_DEFAULT);",
          "dset = H5Dopen (file, \"" <> dataset <> "\", H5P_DEFAULT);",
          "H5Dread (dset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, " <> T.pack address <> ");",
          "H5Fclose (file);",
          "H5Dclose (dset);"
        ]
