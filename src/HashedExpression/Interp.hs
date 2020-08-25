-- |
-- Module      :  HashedExpression.Interp
-- Copyright   :  (c) OCA 2020
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  anandc@mcmaster.ca
-- Stability   :  provisional
-- Portability :  unportable
--
-- Evaluate expressions. Mainly useful for testings.
module HashedExpression.Interp
  ( Evaluable (..),
    evaluate1DReal,
    evaluate1DComplex,
    evaluate2DReal,
    evaluate2DComplex,
    evaluate3DReal,
    evaluate3DComplex,
    fourierTransform1D,
    fourierTransform2D,
    fourierTransform3D,
    FTMode (..),
  )
where

import Data.Array
import Data.Complex
import qualified Data.IntMap.Strict as IM
import Data.List (intercalate)
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import Debug.Trace (traceId, traceShowId)
import GHC.TypeLits (KnownNat)
import HashedExpression.Internal.Expression
  ( C,
    DimSelector (..),
    ET (..),
    Expression (..),
    ExpressionMap,
    NodeID,
    Op (..),
    R,
    Scalar,
  )
import HashedExpression.Internal.Node
import HashedExpression.Internal.Utils
import HashedExpression.Prettify (prettify, showExp)
import HashedExpression.Value

expZeroR :: ExpressionMap -> NodeID -> Expression Scalar R
expZeroR = flip Expression

expZeroC :: ExpressionMap -> NodeID -> Expression Scalar C
expZeroC = flip Expression

-- | Choose branch base on condition value.
-- In Decision tree, there are 2 possible outcomes, Head and Tail.
-- The decision of being Head or Tail is made based on the the condition value.
chooseBranch :: [Double] -> Double -> [a] -> a
chooseBranch marks val branches
  | val < head marks = head branches
  | otherwise =
    snd . last . filter ((val >=) . fst) $ zip marks (tail branches)

-- |  eval is our built-in interpreter which serves to verify semantic preservation of rewriting
class Evaluable d rc output | d rc -> output where
  eval :: ValMaps -> Expression d rc -> output

instance Evaluable Scalar R Double where
  eval :: ValMaps -> Expression Scalar R -> Double
  eval valMap e@(Expression n mp)
    | [] <- retrieveShape n mp =
      case retrieveOp n mp of
        Var name ->
          case Map.lookup name valMap of
            Just (VScalar val) -> val
            _ -> error $ "no value associated with the variable " ++ name
        Param name ->
          case Map.lookup name valMap of
            Just (VScalar val) -> val
            _ -> error $ "no value associated with the parameter " ++ name
        Const val -> val
        Sum args -> sum . map (eval valMap . expZeroR mp) $ args --  sum of a scalar is of the type of R
        Mul args -> product . map (eval valMap . expZeroR mp) $ args --  mul of a scalar is of the type of R
        Neg arg -> - (eval valMap $ expZeroR mp arg) --  Unary minus
        Scale arg1 arg2 ->
          eval valMap (expZeroR mp arg1)
            * eval valMap (expZeroR mp arg2)
        Power x arg -> eval valMap (expZeroR mp arg) ** fromIntegral x --  power operator with 2 inputs, power and base
        Div arg1 arg2 ->
          --  division operator with 2 inputs
          eval valMap (expZeroR mp arg1)
            / eval valMap (expZeroR mp arg2)
        Sqrt arg -> sqrt (eval valMap (expZeroR mp arg)) --  square root
        -- trigonometric functions
        Sin arg -> sin (eval valMap (expZeroR mp arg))
        Cos arg -> cos (eval valMap (expZeroR mp arg))
        Tan arg -> tan (eval valMap (expZeroR mp arg))
        Exp arg -> exp (eval valMap (expZeroR mp arg))
        Log arg -> log (eval valMap (expZeroR mp arg))
        Sinh arg -> sinh (eval valMap (expZeroR mp arg))
        Cosh arg -> cosh (eval valMap (expZeroR mp arg))
        Tanh arg -> tanh (eval valMap (expZeroR mp arg))
        Asin arg -> asin (eval valMap (expZeroR mp arg))
        Acos arg -> acos (eval valMap (expZeroR mp arg))
        Atan arg -> atan (eval valMap (expZeroR mp arg))
        Asinh arg -> asinh (eval valMap (expZeroR mp arg))
        Acosh arg -> acosh (eval valMap (expZeroR mp arg))
        Atanh arg -> atanh (eval valMap (expZeroR mp arg))
        RealPart arg -> realPart (eval valMap (expZeroC mp arg))
        ImagPart arg -> imagPart (eval valMap (expZeroC mp arg))
        --  Inner product is associating each pair of vectors with a scalar
        InnerProd arg1 arg2 ->
          case retrieveShape arg1 mp of
            [] ->
              --  inner product of Scalar
              eval valMap (expZeroR mp arg1)
                * eval valMap (expZeroR mp arg2)
            [size] ->
              --  inner product of vector that returns the sum over elementwise product of vector elements
              let res1 = evaluate1DReal valMap (mp, arg1)
                  res2 = evaluate1DReal valMap (mp, arg2)
               in sum
                    [ x * y
                      | i <- [0 .. size - 1],
                        let x = res1 ! i,
                        let y = res2 ! i
                    ]
            [size1, size2] ->
              --  inner product returns the sum over elementwise product of 2D matrix elements
              let res1 = evaluate2DReal valMap (mp, arg1)
                  res2 = evaluate2DReal valMap (mp, arg2)
               in sum
                    [ x * y
                      | i <- [0 .. size1 - 1],
                        j <- [0 .. size2 - 1],
                        let x = res1 ! (i, j),
                        let y = res2 ! (i, j)
                    ]
            [size1, size2, size3] ->
              --  inner product returns the sum over elementwise product of 3D matrix elements
              let res1 = evaluate3DReal valMap (mp, arg1)
                  res2 = evaluate3DReal valMap (mp, arg2)
               in sum
                    [ x * y
                      | i <- [0 .. size1 - 1],
                        j <- [0 .. size2 - 1],
                        k <- [0 .. size3 - 1],
                        let x = res1 ! (i, j, k),
                        let y = res2 ! (i, j, k)
                    ]
            _ -> error "4D shape?" --  returns error for more than 3D
        Piecewise marks conditionArg branchArgs ->
          let cdt = eval valMap $ expZeroR mp conditionArg
              branches = map (eval valMap . expZeroR mp) branchArgs
           in chooseBranch marks cdt branches
        Project ds arg -> case (retrieveShape arg mp, ds) of
          ([size], [At i]) ->
            let base = evaluate1DReal valMap (mp, arg)
             in base ! i
          ([size1, size2], [At i, At j]) ->
            let base = evaluate2DReal valMap (mp, arg)
             in base ! (i, j)
          ([size1, size2, size3], [At i, At j, At k]) ->
            let base = evaluate3DReal valMap (mp, arg)
             in base ! (i, j, k)
        _ ->
          error
            ("expression structure Scalar R is wrong " ++ prettify e)
    | otherwise = error "one r but shape is not [] ??"

instance Evaluable Scalar C (Complex Double) where
  eval :: ValMaps -> Expression Scalar C -> Complex Double
  eval valMap e@(Expression n mp)
    | [] <- retrieveShape n mp =
      case retrieveOp n mp of
        Sum args -> sum . map (eval valMap . expZeroC mp) $ args --  sum of a scalar is of the type C
        Mul args -> product . map (eval valMap . expZeroC mp) $ args --  Multiplication of a scalar is of the type C
        Power x arg -> eval valMap (expZeroC mp arg) ** fromIntegral x --  power evaluation of arg to the power of x
        Neg arg -> - (eval valMap $ expZeroC mp arg)
        Scale arg1 arg2 ->
          case retrieveElementType arg1 mp of
            R ->
              fromR (eval valMap (expZeroR mp arg1))
                * eval valMap (expZeroC mp arg2)
            C ->
              eval valMap (expZeroC mp arg1)
                * eval valMap (expZeroC mp arg2)
        RealImag arg1 arg2 ->
          --  show the real and imaginary part of complex as x + i y
          eval valMap (expZeroR mp arg1)
            :+ eval valMap (expZeroR mp arg2)
        Conjugate arg -> conjugate $ eval valMap (expZeroC mp arg)
        InnerProd arg1 arg2 ->
          --  evaluate the inner product in C
          case retrieveShape arg1 mp of
            [] ->
              --  evaluation for dimention of zero
              eval valMap (expZeroC mp arg1)
                * conjugate (eval valMap (expZeroC mp arg2))
            [size] ->
              --  inner product evaluation for a vector
              let res1 = evaluate1DComplex valMap $ (mp, arg1)
                  res2 = evaluate1DComplex valMap $ (mp, arg2)
               in sum
                    [ x * conjugate y
                      | i <- [0 .. size - 1],
                        let x = res1 ! i,
                        let y = res2 ! i
                    ]
            [size1, size2] ->
              --  inner product evaluation for 2D matrix
              let res1 = evaluate2DComplex valMap $ (mp, arg1)
                  res2 = evaluate2DComplex valMap $ (mp, arg2)
               in sum
                    [ x * conjugate y
                      | i <- [0 .. size1 - 1],
                        j <- [0 .. size2 - 1],
                        let x = res1 ! (i, j),
                        let y = res2 ! (i, j)
                    ]
            [size1, size2, size3] ->
              --  inner product evaluation for 3D matrix
              let res1 = evaluate3DComplex valMap $ (mp, arg1)
                  res2 = evaluate3DComplex valMap $ (mp, arg2)
               in sum
                    [ x * conjugate y
                      | i <- [0 .. size1 - 1],
                        j <- [0 .. size2 - 1],
                        k <- [0 .. size3 - 1],
                        let x = res1 ! (i, j, k),
                        let y = res2 ! (i, j, k)
                    ]
            _ -> error "4D shape?" --  returns errors for more than 3 dimension
        Piecewise marks conditionArg branchArgs ->
          let cdt = eval valMap $ expZeroR mp conditionArg
              branches = map (eval valMap . expZeroC mp) branchArgs
           in chooseBranch marks cdt branches
        FT arg -> eval valMap (expZeroC mp arg)
        IFT arg -> eval valMap (expZeroC mp arg)
        Project ds arg -> case (retrieveShape arg mp, ds) of
          ([size], [At i]) ->
            let base = evaluate1DComplex valMap (mp, arg)
             in base ! i
          ([size1, size2], [At i, At j]) ->
            let base = evaluate2DComplex valMap (mp, arg)
             in base ! (i, j)
          ([size1, size2, size3], [At i, At j, At k]) ->
            let base = evaluate3DComplex valMap (mp, arg)
             in base ! (i, j, k)
        _ ->
          error
            ("expression structure Scalar C is wrong " ++ prettify e)
    | otherwise = error "One C but shape is not [] ??"

zipWithA :: Ix x => (a -> b -> c) -> Array x a -> Array x b -> Array x c
zipWithA f xs ys = listArray (bounds xs) $ zipWith f (elems xs) (elems ys)

foldrElementwise :: Ix ix => (a -> a -> a) -> [Array ix a] -> Array ix a
foldrElementwise f [x] = x
foldrElementwise f (x : xs) = zipWithA f x (foldrElementwise f xs)

-- | the expression is undefined, here undefined 1D Real expressions are evaluated
evaluate1DReal :: ValMaps -> (ExpressionMap, NodeID) -> Array Int Double
evaluate1DReal valMap (mp, n)
  | [size] <- retrieveShape n mp =
    case retrieveOp n mp of
      Var name ->
        case Map.lookup name valMap of
          Just (V1D val) -> val
          _ -> error $ "no value associated with the variable " ++ name
      Param name ->
        case Map.lookup name valMap of
          Just (V1D val) -> val
          _ -> error $ "no value associated with the parameter " ++ name
      Const val -> listArray (0, size - 1) $ replicate size val
      Sum args ->
        --  evaluate the sum over undefined input arguments
        foldrElementwise (+) . map (evaluate1DReal valMap . (mp,)) $
          args
      Mul args ->
        --  evaluate the Mul over undefined input arguments
        foldrElementwise (*) . map (evaluate1DReal valMap . (mp,)) $
          args
      Power x arg -> fmap (** fromIntegral x) (evaluate1DReal valMap $ (mp, arg)) --  evaluate the power over undefined input arguments
      Neg arg -> fmap negate . evaluate1DReal valMap $ (mp, arg)
      Scale arg1 arg2 ->
        let scalar = eval valMap $ expZeroR mp arg1
         in fmap (scalar *) . evaluate1DReal valMap $ (mp, arg2)
      Div arg1 arg2 ->
        --  evaluate the Division over undefined input arguments
        zipWithA
          (/)
          (evaluate1DReal valMap $ (mp, arg2))
          (evaluate1DReal valMap $ (mp, arg2))
      --  trigonometric functions
      Sqrt arg -> fmap sqrt . evaluate1DReal valMap $ (mp, arg)
      Sin arg -> fmap sin . evaluate1DReal valMap $ (mp, arg)
      Cos arg -> fmap cos . evaluate1DReal valMap $ (mp, arg)
      Tan arg -> fmap tan . evaluate1DReal valMap $ (mp, arg)
      Exp arg -> fmap exp . evaluate1DReal valMap $ (mp, arg)
      Log arg -> fmap log . evaluate1DReal valMap $ (mp, arg)
      Sinh arg -> fmap sinh . evaluate1DReal valMap $ (mp, arg)
      Cosh arg -> fmap cosh . evaluate1DReal valMap $ (mp, arg)
      Tanh arg -> fmap tanh . evaluate1DReal valMap $ (mp, arg)
      Asin arg -> fmap asin . evaluate1DReal valMap $ (mp, arg)
      Acos arg -> fmap acos . evaluate1DReal valMap $ (mp, arg)
      Atan arg -> fmap atan . evaluate1DReal valMap $ (mp, arg)
      Asinh arg -> fmap asinh . evaluate1DReal valMap $ (mp, arg)
      Acosh arg -> fmap acosh . evaluate1DReal valMap $ (mp, arg)
      Atanh arg -> fmap atanh . evaluate1DReal valMap $ (mp, arg)
      RealPart arg -> fmap realPart . evaluate1DComplex valMap $ (mp, arg)
      ImagPart arg -> fmap imagPart . evaluate1DComplex valMap $ (mp, arg)
      -- Rotate rA arg ->
      Piecewise marks conditionArg branchArgs ->
        let cdt = evaluate1DReal valMap $ (mp, conditionArg)
            branches = map (evaluate1DReal valMap . (mp,)) branchArgs
         in listArray
              (0, size - 1)
              [ chosen ! i
                | i <- [0 .. size - 1],
                  let chosen = chooseBranch marks (cdt ! i) branches
              ]
      Rotate [amount] arg ->
        rotate1D size amount (evaluate1DReal valMap $ (mp, arg))
      Project dss arg -> case (retrieveShape arg mp, dss) of
        ([bSize], [ds]) ->
          let base = evaluate1DReal valMap (mp, arg)
           in listArray (0, size - 1) [base ! i | i <- mkIndices ds bSize]
        ([bSize1, bSize2], [ds1, ds2]) ->
          let base = evaluate2DReal valMap (mp, arg)
           in listArray (0, size - 1) [base ! (i, j) | i <- mkIndices ds1 bSize1, j <- mkIndices ds2 bSize2]
        ([bSize1, bSize2, bSize3], [ds1, ds2, ds3]) ->
          let base = evaluate3DReal valMap (mp, arg)
           in listArray
                (0, size - 1)
                [ base ! (i, j, k) | i <- mkIndices ds1 bSize1, j <- mkIndices ds2 bSize2, k <- mkIndices ds3 bSize3
                ]
      Inject [ds] subArg baseArg ->
        let base = evaluate1DReal valMap (mp, baseArg)
            elements = case retrieveShape subArg mp of
              [] -> [eval valMap (expZeroR mp subArg)]
              [_] -> elems $ evaluate1DReal valMap (mp, subArg)
            indices = mkIndices ds size
         in base // zip indices elements
      haha -> error $ "expression structure One R is wrong " ++ show haha
  | otherwise = error "one r but shape is not [size] ??"

instance (KnownNat n) => Evaluable n R (Array Int Double) where
  eval :: ValMaps -> Expression n R -> Array Int Double
  eval valMap (Expression n mp) = evaluate1DReal valMap (mp, n)

-- | evaluate undefined 1D complex expression
evaluate1DComplex ::
  ValMaps -> (ExpressionMap, NodeID) -> Array Int (Complex Double)
evaluate1DComplex valMap (mp, n)
  | [size] <- retrieveShape n mp =
    case retrieveOp n mp of
      Sum args ->
        --  evaluate the sum over undefined input arguments
        foldrElementwise (+) . map (evaluate1DComplex valMap . (mp,)) $
          args
      Mul args ->
        --  evaluate the Mul over undefined input arguments
        foldrElementwise (*) . map (evaluate1DComplex valMap . (mp,)) $
          args
      Power x arg -> fmap (** fromIntegral x) (evaluate1DComplex valMap $ (mp, arg)) --  evaluate the power over undefined input arguments
      Neg arg -> fmap negate . evaluate1DComplex valMap $ (mp, arg)
      Scale arg1 arg2 ->
        case retrieveElementType arg1 mp of
          R ->
            let scalar = fromR . eval valMap $ expZeroR mp arg1
             in fmap (scalar *) . evaluate1DComplex valMap $
                  (mp, arg2)
          C ->
            let scalar = eval valMap $ expZeroC mp arg1
             in fmap (scalar *) . evaluate1DComplex valMap $
                  (mp, arg2)
      RealImag arg1 arg2 ->
        zipWithA
          (:+)
          (evaluate1DReal valMap $ (mp, arg1))
          (evaluate1DReal valMap $ (mp, arg2))
      Conjugate arg ->
        let res = evaluate1DComplex valMap (mp, arg)
         in fmap conjugate res
      Piecewise marks conditionArg branchArgs ->
        let cdt = evaluate1DReal valMap $ (mp, conditionArg)
            branches =
              map (evaluate1DComplex valMap . (mp,)) branchArgs
         in listArray
              (0, size - 1)
              [ chosen ! i
                | i <- [0 .. size - 1],
                  let chosen = chooseBranch marks (cdt ! i) branches
              ]
      Project ds arg -> case (retrieveShape arg mp, ds) of
        ([bSize], [ds]) ->
          let base = evaluate1DComplex valMap (mp, arg)
           in listArray (0, size - 1) [base ! i | i <- mkIndices ds bSize]
        ([bSize1, bSize2], [ds1, ds2]) ->
          let base = evaluate2DComplex valMap (mp, arg)
           in listArray (0, size - 1) [base ! (i, j) | i <- mkIndices ds1 bSize1, j <- mkIndices ds2 bSize2]
        ([bSize1, bSize2, bSize3], [ds1, ds2, ds3]) ->
          let base = evaluate3DComplex valMap (mp, arg)
           in listArray
                (0, size - 1)
                [ base ! (i, j, k) | i <- mkIndices ds1 bSize1, j <- mkIndices ds2 bSize2, k <- mkIndices ds3 bSize3
                ]
      Inject [ds] subArg baseArg ->
        let base = evaluate1DComplex valMap (mp, baseArg)
            elements = case retrieveShape subArg mp of
              [] -> [eval valMap (expZeroC mp subArg)]
              [_] -> elems $ evaluate1DComplex valMap (mp, subArg)
            indices = mkIndices ds size
         in base // zip indices elements
      Rotate [amount] arg ->
        rotate1D size amount (evaluate1DComplex valMap $ (mp, arg))
      FT arg -> fourierTransform1D FT_FORWARD size $ evaluate1DComplex valMap (mp, arg)
      IFT arg -> fourierTransform1D FT_BACKWARD size $ evaluate1DComplex valMap (mp, arg)
      _ -> error "expression structure One C is wrong"
  | otherwise = error "one C but shape is not [size] ??"

instance (KnownNat n) => Evaluable n C (Array Int (Complex Double)) where
  eval :: ValMaps -> Expression n C -> Array Int (Complex Double)
  eval valMap (Expression n mp) = evaluate1DComplex valMap (mp, n)

-- | Evaluate 2D undefined input expressions
evaluate2DReal :: ValMaps -> (ExpressionMap, NodeID) -> Array (Int, Int) Double
evaluate2DReal valMap (mp, n)
  | [size1, size2] <- retrieveShape n mp =
    case retrieveOp n mp of
      Var name ->
        case Map.lookup name valMap of
          Just (V2D val) -> val
          _ -> error $ "no value associated with the variable " ++ name
      Param name ->
        case Map.lookup name valMap of
          Just (V2D val) -> val
          _ -> error $ "no value associated with the parameter " ++ name
      Const val ->
        listArray ((0, 0), (size1 - 1, size2 - 1)) $
          replicate (size1 * size2) val
      Sum args ->
        --  evaluate the sum over undefined input arguments
        foldrElementwise (+) . map (evaluate2DReal valMap . (mp,)) $
          args
      Mul args ->
        --  evaluate the Mul over undefined input arguments
        foldrElementwise (*) . map (evaluate2DReal valMap . (mp,)) $
          args
      Power x arg -> fmap (** fromIntegral x) (evaluate2DReal valMap $ (mp, arg)) --  evaluate the power over undefined input arguments
      Neg arg -> fmap negate . evaluate2DReal valMap $ (mp, arg)
      Scale arg1 arg2 ->
        let scalar = eval valMap $ expZeroR mp arg1
         in fmap (scalar *) . evaluate2DReal valMap $ (mp, arg2)
      Div arg1 arg2 ->
        zipWithA
          (/)
          (evaluate2DReal valMap $ (mp, arg2))
          (evaluate2DReal valMap $ (mp, arg2))
      Sqrt arg -> fmap sqrt . evaluate2DReal valMap $ (mp, arg)
      Sin arg -> fmap sin . evaluate2DReal valMap $ (mp, arg)
      Cos arg -> fmap cos . evaluate2DReal valMap $ (mp, arg)
      Tan arg -> fmap tan . evaluate2DReal valMap $ (mp, arg)
      Exp arg -> fmap exp . evaluate2DReal valMap $ (mp, arg)
      Log arg -> fmap log . evaluate2DReal valMap $ (mp, arg)
      Sinh arg -> fmap sinh . evaluate2DReal valMap $ (mp, arg)
      Cosh arg -> fmap cosh . evaluate2DReal valMap $ (mp, arg)
      Tanh arg -> fmap tanh . evaluate2DReal valMap $ (mp, arg)
      Asin arg -> fmap asin . evaluate2DReal valMap $ (mp, arg)
      Acos arg -> fmap acos . evaluate2DReal valMap $ (mp, arg)
      Atan arg -> fmap atan . evaluate2DReal valMap $ (mp, arg)
      Asinh arg -> fmap asinh . evaluate2DReal valMap $ (mp, arg)
      Acosh arg -> fmap acosh . evaluate2DReal valMap $ (mp, arg)
      Atanh arg -> fmap atanh . evaluate2DReal valMap $ (mp, arg)
      RealPart arg -> fmap realPart . evaluate2DComplex valMap $ (mp, arg)
      ImagPart arg -> fmap imagPart . evaluate2DComplex valMap $ (mp, arg)
      Piecewise marks conditionArg branchArgs ->
        let cdt = evaluate2DReal valMap $ (mp, conditionArg)
            branches = map (evaluate2DReal valMap . (mp,)) branchArgs
         in listArray
              ((0, 0), (size1 - 1, size2 - 1))
              [ chosen ! (i, j)
                | i <- [0 .. size1 - 1],
                  j <- [0 .. size2 - 1],
                  let chosen =
                        chooseBranch marks (cdt ! (i, j)) branches
              ]
      Rotate [amount1, amount2] arg ->
        rotate2D
          (size1, size2)
          (amount1, amount2)
          (evaluate2DReal valMap $ (mp, arg))
      Project ds arg -> case (retrieveShape arg mp, ds) of
        ([bSize1, bSize2], [ds1, ds2]) ->
          let base = evaluate2DReal valMap (mp, arg)
           in listArray
                ((0, 0), (size1 - 1, size2 - 1))
                [ base ! (i, j)
                  | i <- mkIndices ds1 bSize1,
                    j <- mkIndices ds2 bSize2
                ]
        ([bSize1, bSize2, bSize3], [ds1, ds2, ds3]) ->
          let base = evaluate3DReal valMap (mp, arg)
           in listArray
                ((0, 0), (size1 - 1, size2 - 1))
                [ base ! (i, j, k) | i <- mkIndices ds1 bSize1, j <- mkIndices ds2 bSize2, k <- mkIndices ds3 bSize3
                ]
      Inject [ds1, ds2] subArg baseArg ->
        let base = evaluate2DReal valMap (mp, baseArg)
            elements = case retrieveShape subArg mp of
              [] -> [eval valMap (expZeroR mp subArg)]
              [_] -> elems $ evaluate1DReal valMap (mp, subArg)
              [_, _] -> elems $ evaluate2DReal valMap (mp, subArg)
            indices = [(i, j) | i <- mkIndices ds1 size1, j <- mkIndices ds2 size2]
         in base // zip indices elements
      _ -> error "expression structure Two R is wrong"
  | otherwise = error "Two r but shape is not [size1, size2] ??"

instance
  (KnownNat m, KnownNat n) =>
  Evaluable '(m, n) R (Array (Int, Int) Double)
  where
  eval :: ValMaps -> Expression '(m, n) R -> Array (Int, Int) Double
  eval valMap (Expression n mp) = evaluate2DReal valMap (mp, n)

-- | Evaluate 2D undefined complex expression
evaluate2DComplex ::
  ValMaps -> (ExpressionMap, NodeID) -> Array (Int, Int) (Complex Double)
evaluate2DComplex valMap (mp, n)
  | [size1, size2] <- retrieveShape n mp =
    case retrieveOp n mp of
      Sum args ->
        --  evaluate the sum over undefined input arguments
        foldrElementwise (+) . map (evaluate2DComplex valMap . (mp,)) $
          args
      Mul args ->
        --  evaluate the Mul over undefined input arguments
        foldrElementwise (*) . map (evaluate2DComplex valMap . (mp,)) $
          args
      Power x arg -> fmap (** fromIntegral x) (evaluate2DComplex valMap $ (mp, arg)) --  evaluate the power over undefined input arguments
      Neg arg -> fmap negate . evaluate2DComplex valMap $ (mp, arg)
      Scale arg1 arg2 ->
        case retrieveElementType arg1 mp of
          R ->
            let scalar = fromR . eval valMap $ expZeroR mp arg1
             in fmap (scalar *) . evaluate2DComplex valMap $
                  (mp, arg2)
          C ->
            let scalar = eval valMap $ expZeroC mp arg1
             in fmap (scalar *) . evaluate2DComplex valMap $
                  (mp, arg2)
      RealImag arg1 arg2 ->
        zipWithA
          (:+)
          (evaluate2DReal valMap $ (mp, arg1))
          (evaluate2DReal valMap $ (mp, arg2))
      Conjugate arg ->
        let res = evaluate2DComplex valMap (mp, arg)
         in fmap conjugate res
      Piecewise marks conditionArg branchArgs ->
        let cdt = evaluate2DReal valMap $ (mp, conditionArg)
            branches =
              map (evaluate2DComplex valMap . (mp,)) branchArgs
         in listArray
              ((0, 0), (size1 - 1, size2 - 1))
              [ chosen ! (i, j)
                | i <- [0 .. size1 - 1],
                  j <- [0 .. size2 - 1],
                  let chosen =
                        chooseBranch marks (cdt ! (i, j)) branches
              ]
      Rotate [amount1, amount2] arg ->
        rotate2D
          (size1, size2)
          (amount1, amount2)
          (evaluate2DComplex valMap $ (mp, arg))
      FT arg -> fourierTransform2D FT_FORWARD (size1, size2) $ evaluate2DComplex valMap (mp, arg)
      IFT arg -> fourierTransform2D FT_BACKWARD (size1, size2) $ evaluate2DComplex valMap (mp, arg)
      Project ds arg -> case (retrieveShape arg mp, ds) of
        ([bSize1, bSize2], [ds1, ds2]) ->
          let base = evaluate2DComplex valMap (mp, arg)
           in listArray
                ((0, 0), (size1 - 1, size2 - 1))
                [ base ! (i, j)
                  | i <- mkIndices ds1 bSize1,
                    j <- mkIndices ds2 bSize2
                ]
        ([bSize1, bSize2, bSize3], [ds1, ds2, ds3]) ->
          let base = evaluate3DComplex valMap (mp, arg)
           in listArray
                ((0, 0), (size1 - 1, size2 - 1))
                [ base ! (i, j, k) | i <- mkIndices ds1 bSize1, j <- mkIndices ds2 bSize2, k <- mkIndices ds3 bSize3
                ]
      Inject [ds1, ds2] subArg baseArg ->
        let base = evaluate2DComplex valMap (mp, baseArg)
            elements = case retrieveShape subArg mp of
              [] -> [eval valMap (expZeroC mp subArg)]
              [_] -> elems $ evaluate1DComplex valMap (mp, subArg)
              [_, _] -> elems $ evaluate2DComplex valMap (mp, subArg)
            indices = [(i, j) | i <- mkIndices ds1 size1, j <- mkIndices ds2 size2]
         in base // zip indices elements
      _ -> error "expression structure Two C is wrong"
  | otherwise = error "Two C but shape is not [size1, size2] ??"

instance
  (KnownNat m, KnownNat n) =>
  Evaluable '(m, n) C (Array (Int, Int) (Complex Double))
  where
  eval ::
    ValMaps -> Expression '(m, n) C -> Array (Int, Int) (Complex Double)
  eval valMap (Expression n mp) = evaluate2DComplex valMap (mp, n)

-- | Evaluate 3D undefined complex expression
evaluate3DReal ::
  ValMaps -> (ExpressionMap, NodeID) -> Array (Int, Int, Int) Double
evaluate3DReal valMap (mp, n)
  | [size1, size2, size3] <- retrieveShape n mp =
    case retrieveOp n mp of
      Var name ->
        case Map.lookup name valMap of
          Just (V3D val) -> val
          _ -> error "no value associated with the variable"
      Param name ->
        case Map.lookup name valMap of
          Just (V3D val) -> val
          _ -> error "no value associated with the parameter"
      Const val ->
        listArray ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1)) $
          replicate (size1 * size2 * size3) val
      Sum args ->
        foldrElementwise (+) . map (evaluate3DReal valMap . (mp,)) $
          args
      Mul args ->
        foldrElementwise (*) . map (evaluate3DReal valMap . (mp,)) $
          args
      Power x arg -> fmap (** fromIntegral x) (evaluate3DReal valMap $ (mp, arg)) --  evaluate the power over undefined input arguments
      Neg arg -> fmap negate . evaluate3DReal valMap $ (mp, arg)
      Scale arg1 arg2 ->
        let scalar = eval valMap $ expZeroR mp arg1
         in fmap (scalar *) . evaluate3DReal valMap $ (mp, arg2)
      Div arg1 arg2 ->
        zipWithA
          (/)
          (evaluate3DReal valMap $ (mp, arg2))
          (evaluate3DReal valMap $ (mp, arg2))
      Sqrt arg -> fmap sqrt . evaluate3DReal valMap $ (mp, arg)
      Sin arg -> fmap sin . evaluate3DReal valMap $ (mp, arg)
      Cos arg -> fmap cos . evaluate3DReal valMap $ (mp, arg)
      Tan arg -> fmap tan . evaluate3DReal valMap $ (mp, arg)
      Exp arg -> fmap exp . evaluate3DReal valMap $ (mp, arg)
      Log arg -> fmap log . evaluate3DReal valMap $ (mp, arg)
      Sinh arg -> fmap sinh . evaluate3DReal valMap $ (mp, arg)
      Cosh arg -> fmap cosh . evaluate3DReal valMap $ (mp, arg)
      Tanh arg -> fmap tanh . evaluate3DReal valMap $ (mp, arg)
      Asin arg -> fmap asin . evaluate3DReal valMap $ (mp, arg)
      Acos arg -> fmap acos . evaluate3DReal valMap $ (mp, arg)
      Atan arg -> fmap atan . evaluate3DReal valMap $ (mp, arg)
      Asinh arg -> fmap asinh . evaluate3DReal valMap $ (mp, arg)
      Acosh arg -> fmap acosh . evaluate3DReal valMap $ (mp, arg)
      Atanh arg -> fmap atanh . evaluate3DReal valMap $ (mp, arg)
      RealPart arg -> fmap realPart . evaluate3DComplex valMap $ (mp, arg)
      ImagPart arg -> fmap imagPart . evaluate3DComplex valMap $ (mp, arg)
      Piecewise marks conditionArg branchArgs ->
        let cdt = evaluate3DReal valMap $ (mp, conditionArg)
            branches = map (evaluate3DReal valMap . (mp,)) branchArgs
         in listArray
              ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1))
              [ chosen ! (i, j, k)
                | i <- [0 .. size1 - 1],
                  j <- [0 .. size2 - 1],
                  k <- [0 .. size3 - 1],
                  let chosen =
                        chooseBranch marks (cdt ! (i, j, k)) branches
              ]
      Rotate [amount1, amount2, amount3] arg ->
        rotate3D
          (size1, size2, size3)
          (amount1, amount2, amount3)
          (evaluate3DReal valMap $ (mp, arg))
      Project ds arg -> case (retrieveShape arg mp, ds) of
        ([bSize1, bSize2, bSize3], [ds1, ds2, ds3]) ->
          let base = evaluate3DReal valMap (mp, arg)
           in listArray
                ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1))
                [ base ! (i, j, k) | i <- mkIndices ds1 bSize1, j <- mkIndices ds2 bSize2, k <- mkIndices ds3 bSize3
                ]
      Inject [ds1, ds2, ds3] subArg baseArg ->
        let base = evaluate3DReal valMap (mp, baseArg)
            elements = case retrieveShape subArg mp of
              [] -> [eval valMap (expZeroR mp subArg)]
              [_] -> elems $ evaluate1DReal valMap (mp, subArg)
              [_, _] -> elems $ evaluate2DReal valMap (mp, subArg)
              [_, _, _] -> elems $ evaluate3DReal valMap (mp, subArg)
            indices = [(i, j, k) | i <- mkIndices ds1 size1, j <- mkIndices ds2 size2, k <- mkIndices ds3 size3]
         in base // zip indices elements
      _ -> error "expression structure Three R is wrong"
  | otherwise = error "Three r but shape is not [size1, size2, size3] ??"

instance
  (KnownNat m, KnownNat n, KnownNat p) =>
  Evaluable '(m, n, p) R (Array (Int, Int, Int) Double)
  where
  eval :: ValMaps -> Expression '(m, n, p) R -> Array (Int, Int, Int) Double
  eval valMap (Expression n mp) = evaluate3DReal valMap (mp, n)

-- | Evaluate the 3D undefined expression
evaluate3DComplex ::
  ValMaps -> (ExpressionMap, NodeID) -> Array (Int, Int, Int) (Complex Double)
evaluate3DComplex valMap (mp, n)
  | [size1, size2, size3] <- retrieveShape n mp =
    case retrieveOp n mp of
      Sum args ->
        --  evaluate the sum over undefined input arguments
        foldrElementwise (+) . map (evaluate3DComplex valMap . (mp,)) $
          args
      Mul args ->
        --  evaluate the Mul over undefined input arguments
        foldrElementwise (*) . map (evaluate3DComplex valMap . (mp,)) $
          args
      Power x arg -> fmap (** fromIntegral x) (evaluate3DComplex valMap $ (mp, arg)) --  evaluate the power over undefined input arguments
      Neg arg -> fmap negate . evaluate3DComplex valMap $ (mp, arg)
      Scale arg1 arg2 ->
        case retrieveElementType arg1 mp of
          R ->
            let scalar = fromR . eval valMap $ expZeroR mp arg1
             in fmap (scalar *) . evaluate3DComplex valMap $
                  (mp, arg2)
          C ->
            let scalar = eval valMap $ expZeroC mp arg1
             in fmap (scalar *) . evaluate3DComplex valMap $
                  (mp, arg2)
      RealImag arg1 arg2 ->
        zipWithA
          (:+)
          (evaluate3DReal valMap $ (mp, arg1))
          (evaluate3DReal valMap $ (mp, arg2))
      Piecewise marks conditionArg branchArgs ->
        let cdt = evaluate3DReal valMap $ (mp, conditionArg)
            branches =
              map (evaluate3DComplex valMap . (mp,)) branchArgs
         in listArray
              ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1))
              [ chosen ! (i, j, k)
                | i <- [0 .. size1 - 1],
                  j <- [0 .. size2 - 1],
                  k <- [0 .. size3 - 1],
                  let chosen =
                        chooseBranch marks (cdt ! (i, j, k)) branches
              ]
      Rotate [amount1, amount2, amount3] arg ->
        rotate3D
          (size1, size2, size3)
          (amount1, amount2, amount3)
          (evaluate3DComplex valMap $ (mp, arg))
      FT arg -> fourierTransform3D FT_FORWARD (size1, size2, size3) $ evaluate3DComplex valMap (mp, arg)
      IFT arg -> fourierTransform3D FT_BACKWARD (size1, size2, size3) $ evaluate3DComplex valMap (mp, arg)
      Project ds arg -> case (retrieveShape arg mp, ds) of
        ([bSize1, bSize2, bSize3], [ds1, ds2, ds3]) ->
          let base = evaluate3DComplex valMap (mp, arg)
           in listArray
                ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1))
                [ base ! (i, j, k) | i <- mkIndices ds1 bSize1, j <- mkIndices ds2 bSize2, k <- mkIndices ds3 bSize3
                ]
      Inject [ds1, ds2, ds3] subArg baseArg ->
        let base = evaluate3DComplex valMap (mp, baseArg)
            elements = case retrieveShape subArg mp of
              [] -> [eval valMap (expZeroC mp subArg)]
              [_] -> elems $ evaluate1DComplex valMap (mp, subArg)
              [_, _] -> elems $ evaluate2DComplex valMap (mp, subArg)
              [_, _, _] -> elems $ evaluate3DComplex valMap (mp, subArg)
            indices = [(i, j, k) | i <- mkIndices ds1 size1, j <- mkIndices ds2 size2, k <- mkIndices ds3 size3]
         in base // zip indices elements
      _ -> error "expression structure Three C is wrong"
  | otherwise = error "Three C but shape is not [size1, size2, size3] ??"

instance
  (KnownNat m, KnownNat n, KnownNat p) =>
  Evaluable '(m, n, p) C (Array (Int, Int, Int) (Complex Double))
  where
  eval ::
    ValMaps ->
    Expression '(m, n, p) C ->
    Array (Int, Int, Int) (Complex Double)
  eval valMap (Expression n mp) = evaluate3DComplex valMap (mp, n)

-- | Helper functions

-- NOTE: `mod` in Haskell works with negative number, e.g, (-5) `mod` 3 = 1

-- | One dimension rotation.
--   The elemnts falling off of the length of the 1D array will appear at the beginning of the array
rotate1D ::
  -- | Size of the input array
  Int ->
  -- | amount of rotation
  Int ->
  -- | Input array
  Array Int a ->
  -- | Rotated array
  Array Int a
rotate1D size amount arr =
  listArray
    (0, size - 1)
    [arr ! ((i - amount) `mod` size) | i <- [0 .. size - 1]]

-- | Two dimension rotation.
--   The elements falling off of the length of the 2D array will appear at the beginning of the each row or column of the array
rotate2D ::
  -- | Size of the 2d input array
  (Int, Int) ->
  -- | amount of rotation for 2d array
  (Int, Int) ->
  -- | Input 2d array
  Array (Int, Int) a ->
  -- | Rotated 2d array
  Array (Int, Int) a
rotate2D (size1, size2) (amount1, amount2) arr =
  listArray
    ((0, 0), (size1 - 1, size2 - 1))
    [ arr ! ((i - amount1) `mod` size1, (j - amount2) `mod` size2)
      | i <- [0 .. size1 - 1],
        j <- [0 .. size2 - 1]
    ]

-- | Three dimension rotation
rotate3D ::
  -- | Size of 3d input array
  (Int, Int, Int) ->
  -- | Amount of Rotation for 3d array
  (Int, Int, Int) ->
  -- | Input 3d array
  Array (Int, Int, Int) a ->
  -- | Rotated 3d array
  Array (Int, Int, Int) a
rotate3D (size1, size2, size3) (amount1, amount2, amount3) arr =
  listArray
    ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1))
    [ arr
        ! ( (i - amount1) `mod` size1,
            (j - amount2) `mod` size2,
            (k - amount3) `mod` size3
          )
      | i <- [0 .. size1 - 1],
        j <- [0 .. size2 - 1],
        k <- [0 .. size3 - 1]
    ]

data FTMode = FT_FORWARD | FT_BACKWARD deriving (Eq, Ord)

-- | Fourier Transform in 1D.
--  Frequency is just in one dimension.
--  Consider a real-valued function, S(x),
--  that is integrable on an interval of P, which will be the period of the Fourier series.
--  number of cycles is n.
--  length of cycle is P/n, and frequency is n/P.
--  so for input i the frequency is (2*pi*i*n)/P
fourierTransform1D ::
  FTMode -> Int -> Array Int (Complex Double) -> Array Int (Complex Double)
fourierTransform1D mode size arr =
  listArray (0, size - 1) [computeX i | i <- [0 .. size - 1]]
  where
    s = if mode == FT_BACKWARD then fromIntegral size else 1
    computeX i = (sum $ zipWithA (*) arr (fourierBasis i)) / s
    fourierBasis i =
      let frequency n = (2 * pi * fromIntegral (i * n) / fromIntegral size) * (if mode == FT_BACKWARD then -1 else 1)
       in listArray
            (0, size - 1)
            [ cos (frequency n) :+ (- sin (frequency n))
              | n <- [0 .. size - 1]
            ]

-- | Fourier Transform in 2D
--  the frequency should be calculated in 2D
--  Consider a real-valued function, S(x),
--  that is integrable on an interval of P, which will be the period of the Fourier series.
--  numbber of cycles is n.
--  length of cycle is P/n, and frequency is n/P.
--  so for input i the frequency is (2*pi*i*n)/P
--  the frequency should be calculated in both dimensions for i and j
fourierTransform2D ::
  FTMode ->
  (Int, Int) ->
  Array (Int, Int) (Complex Double) ->
  Array (Int, Int) (Complex Double)
fourierTransform2D mode (size1, size2) arr =
  listArray
    ((0, 0), (size1 - 1, size2 - 1))
    [computeX i j | i <- [0 .. size1 - 1], j <- [0 .. size2 - 1]]
  where
    s = if mode == FT_BACKWARD then fromIntegral (size1 * size2) else 1
    computeX i j = (sum $ zipWithA (*) arr (fourierBasis i j)) / s
    fourierBasis i j =
      let frequency m n =
            ( 2 * pi * fromIntegral (i * m) / fromIntegral size1
                + 2 * pi * fromIntegral (j * n) / fromIntegral size2
            )
              * (if mode == FT_BACKWARD then -1 else 1)
       in listArray
            ((0, 0), (size1 - 1, size2 - 1))
            [ cos (frequency m n) :+ (- sin (frequency m n))
              | m <- [0 .. size1 - 1],
                n <- [0 .. size2 - 1]
            ]

-- | Fourier Transform in 3D
--   the frequency should be calculated in 3D
--   Consider a real-valued function, S(x),
--   that is integrable on an interval of P, which will be the period of the Fourier series.
--   numbber of cycles is n.
--   length of cycle is P/n, and frequency is n/P.
--   so for input i the frequency is (2*pi*i*n)/P
--   the frequency should be calculated for all dimensions, i , j , k
fourierTransform3D ::
  FTMode ->
  (Int, Int, Int) ->
  Array (Int, Int, Int) (Complex Double) ->
  Array (Int, Int, Int) (Complex Double)
fourierTransform3D mode (size1, size2, size3) arr =
  listArray
    ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1))
    [ computeX i j k
      | i <- [0 .. size1 - 1],
        j <- [0 .. size2 - 1],
        k <- [0 .. size3 - 1]
    ]
  where
    s = if mode == FT_BACKWARD then fromIntegral (size1 * size2) else 1
    computeX i j k = (sum $ zipWithA (*) arr (fourierBasis i j k)) / s
    fourierBasis i j k =
      let frequency m n p =
            ( 2 * pi * fromIntegral (i * m) / fromIntegral size1
                + 2 * pi * fromIntegral (j * n) / fromIntegral size2
                + 2 * pi * fromIntegral (k * p) / fromIntegral size3
            )
              * (if mode == FT_BACKWARD then -1 else 1)
       in listArray
            ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1))
            [ cos (frequency m n p) :+ (- sin (frequency m n p))
              | m <- [0 .. size1 - 1],
                n <- [0 .. size2 - 1],
                p <- [0 .. size3 - 1]
            ]
