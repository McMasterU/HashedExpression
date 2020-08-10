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
  concatMap genCode $ topologicalSortManyRoots (cExpressionMap, rootIDs)
  where
    shapeOf nID = retrieveShape nID cExpressionMap
    elementTypeOf nID = retrieveElementType nID cExpressionMap
    addressOf :: NodeID -> Text
    addressOf nID = [I.i|(ptr + #{cAddress nID})|]
    [i, j, k, nooffset] = ["i", "j", "k", "0"]
    len nID = product (retrieveShape nID cExpressionMap)
    genCode :: NodeID -> Code
    genCode n =
      let (shape, et, op) = retrieveNode n cExpressionMap
       in case op of
            Var _ -> []
            Param _ -> []
            Const val -> for i (len n) [[I.i|#{n !! i} = #{val};|]]
            Sum args
              | et == R ->
                let sumAt i = T.intercalate " + " $ map (!! i) args
                 in for i (len n) [[I.i|#{n !! i} = #{sumAt i};|]]
              | et == C ->
                let sumReAt i = T.intercalate " + " $ map (`reAt` i) args
                    sumImAt i = T.intercalate " + " $ map (`imAt` i) args
                 in for i (len n) $
                      [ [I.i|#{n `reAt` i} = #{sumReAt i};|],
                        [I.i|#{n `imAt` i} = #{sumImAt i};|]
                      ]
            Mul args
              | et == R ->
                let prodAt i = T.intercalate " * " $ map (!! i) args
                 in for i (len n) [[I.i|#{n !! i} = #{prodAt i};|]]
              | et == C ->
                let prodAt i = T.intercalate " * " $ map (\x -> [I.i|((#{x `reAt` i}) + (#{x `imAt` i}) * I)|]) args
                 in for i (len n) $
                      [ [I.i|double complex res = #{prodAt i};|],
                        [I.i|#{n `reAt` i} = creal(res);|],
                        [I.i|#{n `imAt` i} = cimag(res);|]
                      ]
            Power x arg
              | et == R -> for i (len n) [[I.i|#{n !! i} = pow(#{arg !! i}, #{x});|]]
              | et == C ->
                for i (len n) $
                  [ [I.i|double complex res = cpow((#{arg `reAt` i}) + (#{arg `imAt` i}) * I, #{x});|],
                    [I.i|#{n `reAt` i} = creal(res);|],
                    [I.i|#{n `imAt` i} = cimag(res);|]
                  ]
            Neg arg
              | et == R -> for i (len n) [[I.i|#{n !! i} = - #{arg !! i};|]]
              | et == C ->
                for i (len n) $
                  [ [I.i|#{n `reAt` i} = - #{arg `reAt` i};|],
                    [I.i|#{n `imAt` i} = - #{arg `imAt` i};|]
                  ]
            Scale scalar arg
              | et == R -> for i (len n) [[I.i|#{n !! i} = #{scalar !! nooffset} * #{arg !! i};|]]
              | et == C,
                retrieveElementType scalar cExpressionMap == R ->
                for i (len n) $
                  [ [I.i|#{n `reAt` i} = #{scalar !! nooffset} * #{arg `reAt` i};|],
                    [I.i|#{n `imAt` i} = #{scalar !! nooffset} * #{arg `imAt` i};|]
                  ]
              | et == C,
                retrieveElementType scalar cExpressionMap == C ->
                for i (len n) $
                  [ [I.i|double complex s = (#{scalar `reAt` nooffset}) + (#{scalar `imAt` nooffset}) * I;|],
                    [I.i|double complex res = s * (#{arg `reAt` i} + #{arg `imAt` i} * I);|],
                    [I.i|#{n `reAt` i} = creal(res);|],
                    [I.i|#{n `imAt` i} = cimag(res);|]
                  ]
            Div arg1 arg2
              | et == R -> for i (len n) [[I.i|#{n !! i} = #{arg1 !! i} / #{arg2 !! i};|]]
              | et == C ->
                for i (len n) $
                  [ [I.i|double complex res = ((#{arg1 `reAt` i}) + (#{arg1 `imAt` i}) * I) / ((#{arg2 `reAt` i}) + (#{arg2 `imAt` i}) * I);|],
                    [I.i|#{n `reAt` i} = creal(res);|],
                    [I.i|#{n `imAt` i} = cimag(res);|]
                  ]
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
              for i (len n) $
                [ [I.i|#{n `reAt` i} = #{arg1 !! i};|],
                  [I.i|#{n `imAt` i} = #{arg2 !! i};|]
                ]
            RealPart arg -> for i (len n) [[I.i|#{n !! i} = #{arg `reAt` i};|]]
            ImagPart arg -> for i (len n) [[I.i|#{n !! i} = #{arg `imAt` i};|]]
            Conjugate arg ->
              for i (len n) $
                [ [I.i|#{n `reAt` i} = #{arg `reAt` i};|],
                  [I.i|#{n `imAt` i} = -#{arg `imAt` i};|]
                ]
            InnerProd arg1 arg2
              | et == R && null (shapeOf arg1) -> [[I.i|#{n !! nooffset} = #{arg1 !! nooffset} * #{arg2 !! nooffset};|]]
              | et == R ->
                let initCodes = [[I.i|double acc = 0;|]]
                    codes = for i (len arg1) [[I.i|acc += #{arg1 !! i} * #{arg2 !! i};|]]
                    afterCodes = [[I.i|#{n !! nooffset} = acc;|]]
                 in scoped $ initCodes ++ codes ++ afterCodes
              -- Conjugate the second operand
              | et == C && null (shapeOf arg1) ->
                scoped
                  [ [I.i|double complex res = ((#{arg1 `reAt` nooffset}) + (#{arg1 `imAt` nooffset}) * I) * ((#{arg2 `reAt` nooffset}) - (#{arg2 `imAt` nooffset}) * I);|],
                    [I.i|#{n `reAt` nooffset} = creal(res);|],
                    [I.i|#{n `imAt` nooffset} = cimag(res);|]
                  ]
              | et == C ->
                let initCodes = [[I.i|double complex acc = 0 + 0 * I;|]]
                    codes = for i (len arg1) [[I.i|acc += ((#{arg1 `reAt` i}) + (#{arg1 `imAt` i}) * I) * ((#{arg2 `reAt` i}) - (#{arg2 `imAt` i}) * I);|]]
                    afterCodes =
                      [ [I.i|#{n `reAt` nooffset} = creal(acc);|],
                        [I.i|#{n `imAt` nooffset} = cimag(acc);|]
                      ]
                 in scoped $ initCodes ++ codes ++ afterCodes
            Piecewise marks condition branches ->
              let m : ms = map showT marks
                  Just (b : bs, lst) = viewR branches
                  elseifEach (m, b) =
                    elseif
                      [I.i|#{condition !! i} <= #{m}|]
                      ( if et == R
                          then [[I.i|#{n !! i} = #{b !! i};|]]
                          else
                            [ [I.i|#{n `reAt` i} = #{b `reAt` i};|],
                              [I.i|#{n `imAt` i} = #{b `imAt` i};|]
                            ]
                      )
               in for i (len n) $
                    if_
                      [I.i|#{condition !! i} <= #{m}|]
                      ( if et == R
                          then [[I.i|#{n !! i} = #{b !! i};|]]
                          else
                            [ [I.i|#{n `reAt` i} = #{b `reAt` i};|],
                              [I.i|#{n `imAt` i} = #{b `imAt` i};|]
                            ]
                      )
                      ++ concatMap elseifEach (zip ms bs)
                      ++ else_
                        ( if et == R
                            then [[I.i|#{n !! i} = #{lst !! i};|]]
                            else
                              [ [I.i|#{n `reAt` i} = #{lst `reAt` i};|],
                                [I.i|#{n `imAt` i} = #{lst `imAt` i};|]
                              ]
                        )
            Rotate [amount] arg ->
              let [size] = shape
               in for i size $
                    [[I.i|int ai = (#{i} - #{amount} + #{size}) % #{size};|]]
                      ++ ( if et == R
                             then [[I.i|#{n !! i} = #{arg !! "ai"};|]]
                             else
                               [ [I.i|#{n `reAt` i} = #{arg `reAt` "ai"};|],
                                 [I.i|#{n `imAt` i} = #{arg `imAt` "ai"};|]
                               ]
                         )
            Rotate [amount1, amount2] arg ->
              let [size1, size2] = shape
                  toIndex i j = [I.i|#{i} * #{size2} + #{j}|]
               in for i size1 $
                    for j size2 $
                      [ [I.i|int ai = (#{i} - #{amount1} + #{size1}) % #{size1};|],
                        [I.i|int aj = (#{j} - #{amount2} + #{size2}) % #{size2};|]
                      ]
                        ++ ( if et == R
                               then [[I.i|#{n !! (toIndex i j)} = #{arg !! (toIndex "ai" "aj")};|]]
                               else
                                 [ [I.i|#{n `reAt` (toIndex i j)} = #{arg `reAt` (toIndex "ai" "aj")};|],
                                   [I.i|#{n `imAt` (toIndex i j)} = #{arg `imAt` (toIndex "ai" "aj")};|]
                                 ]
                           )
            Rotate [amount1, amount2, amount3] arg ->
              let [size1, size2, size3] = shape
                  toIndex i j k = [I.i|#{i} * #{size2} * #{size3} + #{j} * #{size3} + #{k}|]
               in for i size1 $
                    for j size2 $
                      for k size3 $
                        [ [I.i|int ai = (#{i} - #{amount1} + #{size1}) % #{size1};|],
                          [I.i|int aj = (#{j} - #{amount2} + #{size2}) % #{size2};|],
                          [I.i|int ak = (#{k} - #{amount3} + #{size3}) % #{size3}'|]
                        ]
                          ++ ( if et == R
                                 then [[I.i|#{n !! (toIndex i j k)} = #{arg !! (toIndex "ai" "aj" "ak")};|]]
                                 else
                                   [ [I.i|#{n `reAt` (toIndex i j k)} = #{arg `reAt` (toIndex "ai" "aj" "ak")};|],
                                     [I.i|#{n `imAt` (toIndex i j k)} = #{arg `imAt` (toIndex "ai" "aj" "ak")};|]
                                   ]
                             )
            FT arg ->
              case shape of
                [size] -> [[I.i|dft_1d(#{size}, #{addressOf arg}, #{addressOf n}, FFTW_FORWARD);|]]
                [size1, size2] -> [[I.i|dft_2d(#{size1}, #{size2}, #{addressOf arg}, #{addressOf n}, FFTW_FORWARD);|]]
            IFT arg ->
              case shape of
                [size] -> [[I.i|dft_1d(#{size}, #{addressOf arg}, #{addressOf n}, FFTW_BACKWARD);|]]
                [size1, size2] -> [[I.i|dft_2d(#{size1}, #{size2}, #{addressOf arg}, #{addressOf n}, FFTW_BACKWARD);|]]
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
      variableShapes :: [Shape]
      variableShapes = map (variableShape . varName) variables
      variableSizes :: [Int]
      variableSizes = map product variableShapes
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
      writeResultCodeEach :: Variable -> Code
      writeResultCodeEach variable
        | output == OutputHDF5 =
          scoped
            [ [i|printf("Writing #{name} to #{name}_out.h5...\\n");|],
              "hid_t file, space, dset;",
              [i|hsize_t dims[#{length shape}] = {#{intercalate ", " . map show $ shape}};|],
              [i|file = H5Fcreate("#{name}_out.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);|],
              [i|space = H5Screate_simple (#{length shape}, dims, NULL);|],
              [i|dset = H5Dcreate (file, "#{name}", H5T_IEEE_F64LE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);|],
              [i|H5Dwrite(dset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, ptr + #{offset});|],
              "H5Dclose(dset);",
              "H5Sclose(space);",
              "H5Fclose(file);"
            ]
        | output == OutputText =
          scoped $
            [ [i|printf("Writing #{name} to #{name}_out.txt...\\n");|],
              "FILE *file;",
              [i|file = fopen("#{name}_out.txt", "w");|]
            ]
              ++ ( for "i" (product shape) $
                     [ [i|fprintf(file, "%f ", ptr[#{offset} + i]);|]
                     ]
                 )
              ++ ["fclose(file);"]
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
          ++ scoped (concatMap readValCodeEach varsAndParams)
          ++ ["}"] --
          -------------------------------------------------------------------------------
      writeResultCodes =
        ["void write_result()"]
          ++ scoped (concatMap writeResultCodeEach variables)
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
        [ [i|printf("Reading #{name} from HDF5 file in dataset #{dataset} from #{filePath} ... \\n");|],
          "hid_t file, dset;",
          [i|file = H5Fopen("#{filePath}", H5F_ACC_RDONLY, H5P_DEFAULT);|],
          [i|dset = H5Dopen(file, "#{dataset}", H5P_DEFAULT);|],
          [i|H5Dread (dset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, #{address});|],
          "H5Fclose (file);",
          "H5Dclose (dset);"
        ]
