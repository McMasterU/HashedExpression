module HashedExpression.Codegen.CSimple where

import Data.Array ((!), indices)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', partition, tails)
import Data.List.HT (viewR)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.String.Interpolate
import qualified Data.String.Interpolate as I
import Data.Text (Text)
import qualified Data.Text as T
import HashedExpression.Codegen
import HashedExpression.Embed.FFTW (fftUtils)
import HashedExpression.Internal.Expression (DimensionType, ET (..), Expression, ExpressionMap, Node (..), NumType, Shape, exMap)
import HashedExpression.Internal.Inner (containsFTNode, topologicalSortManyRoots, unwrap)
import HashedExpression.Internal.Node (nodeElementType, retrieveElementType, retrieveInternal, retrieveNode, retrieveShape)
import HashedExpression.Internal.Utils (Val (..), ValMaps, showT)
import Prelude hiding ((!!))

-------------------------------------------------------------------------------

-- | Generate simple C code
data CSimpleConfig = CSimpleConfig

type Address = Int

type NodeID = Int

data CSimpleCodegen
  = CSimpleCodegen
      { expressionMap :: ExpressionMap,
        address :: NodeID -> Address,
        memSize :: Int,
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
      { expressionMap = mp,
        address = addressMap,
        memSize = totalSize,
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
  assigningValues :: CSimpleCodegen -> ValMaps -> Code
  assigningValues CSimpleCodegen {..} valMaps = concatMap codesForVar vars
    where
      [i, j, k, nooffset] = ["j", "k", "l", "0"]
      vars :: [(Int, String)]
      vars =
        let toVar nId
              | Var varName <- retrieveNode nId expressionMap = Just (nId, varName)
              | otherwise = Nothing
         in mapMaybe toVar . IM.keys $ expressionMap
      codesForVar :: (Int, String) -> Code
      codesForVar (n, varName) =
        case Map.lookup varName valMaps of
          Just (VScalar val) -> [[I.i|#{n !! nooffset} = #{val};|]]
          Just (V1D array1d) ->
            let assignIndex id = [[I.i|#{n !! (showT id)} = #{array1d ! id};|]]
             in concatMap assignIndex $ indices array1d
          Just (V2D array2d) ->
            let shape = retrieveShape n expressionMap
                assignIndex (id1, id2) = [[I.i|#{n !! showT (localOffset shape [id1, id2])} = #{array2d ! (id1, id2)};|]]
             in concatMap assignIndex $ indices array2d
          Just (V3D array3d) ->
            let shape = retrieveShape n expressionMap
                assignIndex (id1, id2, id3) = [[I.i|#{n !! showT (localOffset shape [id1, id2, id3])} = #{array3d ! (id1, id2, id2)};|]]
             in concatMap assignIndex $ indices array3d
          _ -> []

  -------------------------------------------------------------------------------
  evaluating :: CSimpleCodegen -> [Int] -> Code
  evaluating CSimpleCodegen {..} rootIDs =
    concatMap genCode $ topologicalSortManyRoots (expressionMap, rootIDs)
    where
      shapeOf nID = retrieveShape nID expressionMap
      elementTypeOf nID = retrieveElementType nID expressionMap
      addressOf :: Int -> Text
      addressOf nID = [I.i|(ptr + #{address nID})|]
      [i, j, k, nooffset] = ["j", "k", "l", "0"]
      len nID = product (retrieveShape nID expressionMap)
      genCode :: Int -> Code
      genCode n =
        let (shape, node) = retrieveInternal n expressionMap
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
                        [I.i|int aj = (#{i} - #{amount2} + #{size2}) % #{size2};|],
                        [I.i|#{n !! (toIndex i j)} = #{arg !! (toIndex "ai" "aj")};|]
                      ]
              Rotate [amount1, amount2, amount3] arg ->
                let [size1, size2, size3] = shape
                    toIndex i j k = [I.i|#{i} * #{size2} * #{size3} + #{j} * #{size3} + #{k}|]
                 in for i size1 $ for j size2 $ for k size3 $
                      [ [I.i|int ai = (#{i} - #{amount1} + #{size1}) % #{size1};|],
                        [I.i|int aj = (#{i} - #{amount2} + #{size2}) % #{size2};|],
                        [I.i|int ak = (#{i} - #{amount3} + #{size3}) % #{size3}'|],
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
              node -> error $ show node

-------------------------------------------------------------------------------

-- | Generate a fully working C program that compute the expression and print out the result
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
    [i, j, k, nooffset] = ["j", "k", "l", "0"]
    initMemory = [[I.i|double *ptr = malloc(sizeof(double) * #{memSize codeGen});|]]
    -- assign value to variables
    assignVals = assigningValues codeGen valMaps
    -- codes to compute
    codes = evaluating codeGen [n]
    -- print the value of expression
    printValue
      | et == R = for i bound [[I.i|printf("%f", #{(!!) codeGen n i});|]]
      | et == C =
        for i bound [[I.i|printf("%f", #{(!!) codeGen n i});|]]
          ++ [[I.i|printf("\n");|]]
          ++ for i bound [[I.i|printf("%f", #{imAt codeGen n i});|]]
    releaseMemory = ["free(ptr);"]
