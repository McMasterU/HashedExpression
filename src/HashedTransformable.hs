{-
(c) 2010-2012 Christopher Kumar Anand, Jessica LM Pavlin

Calculating Derivatives.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HashedTransformable
    (
    ) where

import HashedExpression
import HashedFactor
import HashedSimplify

{-

Class for expression types which can be simplified, differentials isolated, etc.
-}
instance Transformable (Internal, Node) where
    factor = factor'
    simplify = simplify'
    simplifyOne = simplify''

instance Transformable Expression where
    factor (Expression n exprs) = Expression n' exprs'
      where
        (exprs', n') = factor' (exprs, n)
    simplify (Expression n exprs) = Expression n' exprs'
      where
        (exprs', n') = simplify' (exprs, n)
    simplifyOne (Expression n exprs) = Expression n' exprs'
      where
        (exprs', n') = simplify'' (exprs, n)

instance Transformable Scalar where
    factor (Scalar (Expression n exprs)) = Scalar $ Expression n' exprs'
      where
        (exprs', n') = factor' (exprs, n)
    simplify (Scalar (Expression n exprs)) = Scalar $ Expression n' exprs'
      where
        (exprs', n') = simplify' (exprs, n)
    simplifyOne (Scalar (Expression n exprs)) = Scalar $ Expression n' exprs'
      where
        (exprs', n') = simplify'' (exprs, n)

instance Transformable ScalarC where
    factor (ScalarC (Expression n exprs)) = ScalarC $ Expression n' exprs'
      where
        (exprs', n') = factor' (exprs, n)
    simplify (ScalarC (Expression n exprs)) = ScalarC $ Expression n' exprs'
      where
        (exprs', n') = simplify' (exprs, n)
    simplifyOne (ScalarC (Expression n exprs)) = ScalarC $ Expression n' exprs'
      where
        (exprs', n') = simplify'' (exprs, n)

instance Transformable OneD where
    factor (OneD (Expression n exprs)) = OneD $ Expression n' exprs'
      where
        (exprs', n') = factor' (exprs, n)
    simplify (OneD (Expression n exprs)) = OneD $ Expression n' exprs'
      where
        (exprs', n') = simplify' (exprs, n)
    simplifyOne (OneD (Expression n exprs)) = OneD $ Expression n' exprs'
      where
        (exprs', n') = simplify'' (exprs, n)

instance Transformable OneDC where
    factor (OneDC (Expression n exprs)) = OneDC $ Expression n' exprs'
      where
        (exprs', n') = factor' (exprs, n)
    simplify (OneDC (Expression n exprs)) = OneDC $ Expression n' exprs'
      where
        (exprs', n') = simplify' (exprs, n)
    simplifyOne (OneDC (Expression n exprs)) = OneDC $ Expression n' exprs'
      where
        (exprs', n') = simplify'' (exprs, n)

instance Transformable TwoD where
    factor (TwoD (Expression n exprs)) = TwoD $ Expression n' exprs'
      where
        (exprs', n') = factor' (exprs, n)
    simplify (TwoD (Expression n exprs)) = TwoD $ Expression n' exprs'
      where
        (exprs', n') = simplify' (exprs, n)
    simplifyOne (TwoD (Expression n exprs)) = TwoD $ Expression n' exprs'
      where
        (exprs', n') = simplify'' (exprs, n)

instance Transformable TwoDC where
    factor (TwoDC (Expression n exprs)) = TwoDC $ Expression n' exprs'
      where
        (exprs', n') = factor' (exprs, n)
    simplify (TwoDC (Expression n exprs)) = TwoDC $ Expression n' exprs'
      where
        (exprs', n') = simplify' (exprs, n)
    simplifyOne (TwoDC (Expression n exprs)) = TwoDC $ Expression n' exprs'
      where
        (exprs', n') = simplify'' (exprs, n)

instance Transformable TwoDSparse where
    factor (TwoDSparse sl (Expression n exprs)) =
        TwoDSparse sl $ Expression n' exprs'
      where
        (exprs', n') = factor' (exprs, n)
    simplify (TwoDSparse sl (Expression n exprs)) =
        TwoDSparse sl $ Expression n' exprs'
      where
        (exprs', n') = simplify' (exprs, n)
    simplifyOne (TwoDSparse sl (Expression n exprs)) =
        TwoDSparse sl $ Expression n' exprs'
      where
        (exprs', n') = simplify'' (exprs, n)

instance Transformable TwoDCSparse where
    factor (TwoDCSparse sl (Expression n exprs)) =
        TwoDCSparse sl $ Expression n' exprs'
      where
        (exprs', n') = factor' (exprs, n)
    simplify (TwoDCSparse sl (Expression n exprs)) =
        TwoDCSparse sl $ Expression n' exprs'
      where
        (exprs', n') = simplify' (exprs, n)
    simplifyOne (TwoDCSparse sl (Expression n exprs)) =
        TwoDCSparse sl $ Expression n' exprs'
      where
        (exprs', n') = simplify'' (exprs, n)

instance Transformable ThreeD where
    factor (ThreeD (Expression n exprs)) = ThreeD $ Expression n' exprs'
      where
        (exprs', n') = factor' (exprs, n)
    simplify (ThreeD (Expression n exprs)) = ThreeD $ Expression n' exprs'
      where
        (exprs', n') = simplify' (exprs, n)
    simplifyOne (ThreeD (Expression n exprs)) = ThreeD $ Expression n' exprs'
      where
        (exprs', n') = simplify'' (exprs, n)

instance Transformable ThreeDC where
    factor (ThreeDC (Expression n exprs)) = ThreeDC $ Expression n' exprs'
      where
        (exprs', n') = factor' (exprs, n)
    simplify (ThreeDC (Expression n exprs)) = ThreeDC $ Expression n' exprs'
      where
        (exprs', n') = simplify' (exprs, n)
    simplifyOne (ThreeDC (Expression n exprs)) = ThreeDC $ Expression n' exprs'
      where
        (exprs', n') = simplify'' (exprs, n)

instance Transformable ThreeDSparse where
    factor (ThreeDSparse sl (Expression n exprs)) =
        ThreeDSparse sl $ Expression n' exprs'
      where
        (exprs', n') = factor' (exprs, n)
    simplify (ThreeDSparse sl (Expression n exprs)) =
        ThreeDSparse sl $ Expression n' exprs'
      where
        (exprs', n') = simplify' (exprs, n)
    simplifyOne (ThreeDSparse sl (Expression n exprs)) =
        ThreeDSparse sl $ Expression n' exprs'
      where
        (exprs', n') = simplify'' (exprs, n)

instance Transformable ThreeDCSparse where
    factor (ThreeDCSparse sl (Expression n exprs)) =
        ThreeDCSparse sl $ Expression n' exprs'
      where
        (exprs', n') = factor' (exprs, n)
    simplify (ThreeDCSparse sl (Expression n exprs)) =
        ThreeDCSparse sl $ Expression n' exprs'
      where
        (exprs', n') = simplify' (exprs, n)
    simplifyOne (ThreeDCSparse sl (Expression n exprs)) =
        ThreeDCSparse sl $ Expression n' exprs'
      where
        (exprs', n') = simplify'' (exprs, n)
{-

-}
