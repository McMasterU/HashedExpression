module HashedExpression.Codegen.CSimple where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', partition)
import Data.List.HT (viewR)
import qualified Data.Set as Set
import Data.String.Interpolate
import qualified Data.String.Interpolate as I
import Data.Text (Text)
import qualified Data.Text as T
import HashedExpression.Codegen
import HashedExpression.Internal.Expression (ET (..), ExpressionMap, Node (..), Shape)
import HashedExpression.Internal.Inner (topologicalSortManyRoots)
import HashedExpression.Internal.Node (nodeElementType, retrieveElementType, retrieveInternal, retrieveShape)
import HashedExpression.Internal.Utils (ValMaps)

-------------------------------------------------------------------------------

-- | Generate simple C code
data CSimpleConfig = CSimpleConfig

type Address = Int

data CSimpleCodegen
  = CSimpleCodegen
      { exMap :: ExpressionMap,
        address :: Int -> Address,
        memSize :: Int
      }

-------------------------------------------------------------------------------

-- | Helpers for code generation
infix 1 <~

(<~) :: Text -> Text -> Text
(<~) a b = [i|#{a} = #{b}|]

scoped :: Code -> Code
scoped codes = ["{"] ++ indent 2 codes ++ ["}"]

ptr :: CSimpleCodegen -> Int -> Text -> Text
ptr (CSimpleCodegen mp address _) nID offsetVal =
  let offset
        | offsetVal == "" = ""
        | offsetVal == "0" = ""
        | otherwise = " + " <> offsetVal
   in [i|ptr[#{address nID} + #{offset}]|]

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

-------------------------------------------------------------------------------
instance Codegen CSimpleCodegen CSimpleConfig where
  initCodegen :: CodegenInit -> CSimpleConfig -> CSimpleCodegen
  initCodegen (CodegenInit mp consecutiveIDs) _ =
    CSimpleCodegen
      { exMap = mp,
        address = addressMap,
        memSize = totalSize
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

  -------------------------------------------------------------------------------
  assigningValues :: CSimpleCodegen -> ValMaps -> Code
  assigningValues = undefined

  -------------------------------------------------------------------------------
  evaluating :: CSimpleCodegen -> [Int] -> Code
  evaluating cgen@(CSimpleCodegen mp address _) rootIDs =
    concatMap genCode $ topologicalSortManyRoots (mp, rootIDs)
    where
      shapeOf nID = retrieveShape nID mp
      elementTypeOf nID = retrieveElementType nID mp
      addressOf :: Int -> Text
      addressOf nID = [I.i|(ptr + #{address nID})|]
      at = ptr cgen
      reAt = at
      imAt nID offset =
        ptr cgen nID $
          offset <> " + "
            <> T.pack (show (product (retrieveShape nID mp)))
      [i, j, k, nooffset] = ["j", "k", "l", "0"]
      genCode :: Int -> Code
      genCode n =
        let (shape, node) = retrieveInternal n mp
            bound = product shape
         in case node of
              Var _ -> []
              Const _ -> []
              Sum R args ->
                let sumAt i = T.intercalate " + " $ map (`at` i) args
                 in for i bound [n `at` i <~ sumAt i]
              Mul R args ->
                let prodAt i = T.intercalate " * " $ map (`at` i) args
                 in for i bound [n `at` i <~ prodAt i]
              Neg R arg ->
                let negAt i = "-" <> arg `at` i
                 in for i bound [n `at` i <~ negAt i]
              Scale R scalar arg ->
                let scaleAt i =
                      (scalar `at` nooffset) <> "*" <> arg `at` i
                 in for i bound [n `at` i <~ scaleAt i]
              Div arg1 arg2 ->
                let divAt i = arg1 `at` i <> " / " <> arg2 `at` i
                 in for i bound [n `at` i <~ divAt i]
              Sqrt arg -> for i bound [n `at` i <~ "sqrt(" <> arg `at` i <> ")"]
              Sin arg -> for i bound [n `at` i <~ "sin(" <> arg `at` i <> ")"]
              Cos arg -> for i bound [n `at` i <~ "cos(" <> arg `at` i <> ")"]
              Tan arg -> for i bound [n `at` i <~ "tan(" <> arg `at` i <> ")"]
              Exp arg -> for i bound [n `at` i <~ "exp(" <> arg `at` i <> ")"]
              Log arg -> for i bound [n `at` i <~ "log(" <> arg `at` i <> ")"]
              Sinh arg -> for i bound [n `at` i <~ "sinh(" <> arg `at` i <> ")"]
              Cosh arg -> for i bound [n `at` i <~ "cosh(" <> arg `at` i <> ")"]
              Tanh arg -> for i bound [n `at` i <~ "tanh(" <> arg `at` i <> ")"]
              Asin arg -> for i bound [n `at` i <~ "asin(" <> arg `at` i <> ")"]
              Acos arg -> for i bound [n `at` i <~ "acos(" <> arg `at` i <> ")"]
              Atan arg -> for i bound [n `at` i <~ "atan(" <> arg `at` i <> ")"]
              Asinh arg -> for i bound [n `at` i <~ "asinh(" <> arg `at` i <> ")"]
              Acosh arg -> for i bound [n `at` i <~ "acosh(" <> arg `at` i <> ")"]
              Atanh arg -> for i bound [n `at` i <~ "atanh(" <> arg `at` i <> ")"]
              RealImag arg1 arg2 ->
                for i bound [n `reAt` i <~ arg1 `at` i]
                  ++ for i bound [n `imAt` i <~ arg2 `at` i]
              InnerProd R arg1 arg2
                | null (shapeOf arg1) ->
                  [n `at` nooffset <~ arg1 `at` nooffset <> "*" <> arg2 `at` nooffset]
                | otherwise ->
                  let initCodes = ["double acc" <~ "0"]
                      codes = for i bound ["acc +=" <> arg1 `at` i <> " * " <> arg2 `at` i]
                      afterCodes = [n `at` nooffset <~ "acc"]
                   in scoped $ initCodes ++ codes ++ afterCodes
              Piecewise marks condition branches ->
                let m : ms = map (T.pack . show) marks
                    Just (b : bs, lst) = viewR branches
                    elseifEach (m, b) =
                      elseif
                        (condition `at` i <> " <= " <> m)
                        [n `at` i <~ b `at` i]
                 in for i bound $
                      if_ (condition `at` i <> " <= " <> m) [n `at` i <~ b `at` i]
                        <> concatMap elseifEach (zip ms bs)
                        <> else_ [n `at` i <~ lst `at` i]
              Rotate [amount] arg ->
                let [size] = shape
                 in for i size $
                      [ [I.i|int ai = (#{i} - #{amount} + #{size}) % #{size}|],
                        n `at` i <~ arg `at` "ai"
                      ]
              Rotate [amount1, amount2] arg ->
                let [size1, size2] = shape
                    toIndex i j = [I.i|#{i} * #{size2} + #{j}|]
                 in for i size1 $ for j size2 $
                      [ [I.i|int ai = (#{i} - #{amount1} + #{size1}) % #{size1}|],
                        [I.i|int aj = (#{i} - #{amount2} + #{size2}) % #{size2}|],
                        n `at` toIndex i j <~ arg `at` toIndex "ai" "aj"
                      ]
              Rotate [amount1, amount2, amount3] arg ->
                let [size1, size2, size3] = shape
                    toIndex i j k = [I.i|#{i} * #{size2} * #{size3} + #{j} * #{size3} + #{k}|]
                 in for i size1 $ for j size2 $ for k size3 $
                      [ [I.i|int ai = (#{i} - #{amount1} + #{size1}) % #{size1}|],
                        [I.i|int aj = (#{i} - #{amount2} + #{size2}) % #{size2}|],
                        [I.i|int ak = (#{i} - #{amount3} + #{size3}) % #{size3}|],
                        n `at` toIndex i j k <~ arg `at` toIndex "ai" "aj" "ak"
                      ]
              TwiceReFT arg ->
                case shape of
                  [size] -> [[I.i|re_dft_twice_1d(#{size}, #{addressOf arg}, #{addressOf n});|]]
                  [size1, size2] -> [[I.i|re_dft_twice_2d(#{size1}, #{size2}, #{addressOf arg}, #{addressOf n});|]]
              TwiceImFT arg ->
                case shape of
                  [size] -> [[I.i|im_dft_twice_1d(#{size}, #{addressOf arg}, #{addressOf n});|]]
                  [size1, size2] -> [[I.i|im_dft_twice_2d(#{size1}, #{size2}, #{addressOf arg}, #{addressOf n});|]]
-------------------------------------------------------------------------------
