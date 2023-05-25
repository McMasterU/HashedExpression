-- |
-- Module      :  Examples.Tutorial
-- Copyright   :  (c) OCA 2023
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  unportable
--
-- Description : Introductory tutorial to the inner workings of Hashed
-- Expression. Its highly recommended to load this module into GHCi and actively
-- play around with it while reading through

module Tutorial where

import HashedExpression
import HashedExpression.Modeling.Typed
import HashedExpression.Prettify
import HashedExpression.Internal
import HashedExpression.Internal.Pattern
import HashedExpression.Differentiation.Reverse (partialDerivativesMap)

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.Maybe (isJust)

-- | Example: Simple Hashed Expression
--
-- Creating an expression in Hashed Expression looks almost completely the same
-- as creating any mathematical expression in Haskell. This is because we've
-- overloaded all the standard @Num@, @Fractional@ and @Floating@ operators.
-- See <HashedExpression/Modeling/Typed.hs> for all supported operators
--
-- We can distinguish an expression like @2*2@ from just any @Num@ instance by
-- giving it the type @TypedExpr@. Note this expression returns a @Scalar@
-- (i.e., a single value not a vector) and is real number (@R@ stands for real
-- and @C@ for complex)
exampleExpr :: TypedExpr Scalar R
exampleExpr =
  let
    -- Hashed Expression supports variable declaration, these variables are
    -- scalars, there are other functions like variable1D for declaring
    -- multi-dimensional variables, parameters or constants (see Typed.hs)
    x1 = variable "x1"
    x2 = variable "x2"
  in (5 * x1) + (2 * x2)

-- | This expression contains only one node, @([],R,Const 4.0)@
constantExpr :: TypedExpr Scalar R
constantExpr = 4

-- | This expression is non-linear
nonLinearExpr :: TypedExpr Scalar R
nonLinearExpr =
  let
    x1 = variable "x1"
  in x1 * (x1 + 2)

-- | The internals of a Hashed Expression is a map from @NodeID@ (i.e, just an
-- Int serving as a unique identifier) to a @Node@ (i.e., a variable, constant
-- or operator that references other @NodeID@ as its arguments).
-- See <HashedExpression/Internal/Base.hs> for these definitions
-- You can access this Map and the @NodeID@ at the "top" of the expression
-- through the function @asRawExpr@
-- NOTE try loading this module into GHCi and running this function with
-- @
-- -- >>> printRawExpr exampleExpr
-- -- >>> printRawExpr constantExpr
-- -- >>> printRawExpr nonLinearExpr
-- @
printRawExpr :: IsExpression e => e -> IO ()
printRawExpr expr =
  let
    (exprMap,nID) = asRawExpr expr
    -- calling varNodes on the expression map will return all the variables with
    -- their labels, shape (i.e., dimensions) and NodeID
    allVariables :: [(String,Shape,NodeID)]
    allVariables = varNodes exprMap
  in putStrLn $ unlines
      ["Expression Map: " ++ show exprMap
      ,"Top Node ID: " ++ show nID
      ,"All Variables: " ++ show allVariables]

-- | To evaluate an expression, you need to provide it with values to plug into
-- variables (or parameters). This is done through a @ValMap@, which is simply a
-- @Map@ from Strings (i.e., variable labels) to their respective values encoded
-- by @Val@. See <HashedExpression/Value.hs> for the definition of @Val@
--
-- NOTE You must ensure you use the proper constructor in @Val@ for the
-- variables dimensions, i.e, if you declare a @variable "x1"@, you should use
-- @VScalar@, but if you declare a @variable1D "x1"@, you should use @V1D@
--
-- The result of the evaluation returns an @InterpValue@ type, see
-- <HashedExpression/Interp.hs> for its definition. The constructor corresponds
-- to the resulting dimensions/type. For example, if the expression is of type
-- @TypedExpr Scalar R@ it will return a @VR Double@
--
-- Try evaluating the following code
-- @
-- -- >>> exampleEval exampleExpr
-- -- >>> exampleEval constantExpr
-- -- >>> exampleEval nonLinearExpr
-- @
exampleEval :: IsExpression e => e -> InterpValue
exampleEval expr =
  let
    valMap = Map.fromList [("x1",VScalar 1.0)
                          ,("x2",VScalar 0.0)]
  in eval valMap expr

-- | Suppose you want to check if an expression is nothing more than a constant.
-- Since a @RawExpr@ is just an @IntMap@ and the top @NodeID@, you can simply
-- use @IntMap.lookup@ to lookup the @NodeID@ and pattern match its @Node@ type.
-- A @Node@ is a tuple of @(Shape,ElementType,Op)@,
-- see <HashedExpression/Internal/Base.hs> for definitions.
-- NOTE you could use a similar method to check if the expression is a variable,
-- or what operation the expression is at some NodeID
-- Try running the code
-- @
-- -- >>> exprIsConstant exampleExpr
-- -- >>> exprIsConstant constantExpr
-- -- >>> exprIsConstant nonLinearExpr
-- @
exprIsConstant :: IsExpression e => e -> Maybe Double
exprIsConstant expr =
  let
    (exprMap,nID) = asRawExpr expr
  in case IntMap.lookup (unNodeID nID) exprMap of
       Just ([],R,Const d) -> Just d
       -- To check if the expression was a variable and return its label you could use
       -- Just (_,_,Var s) -> Just s
       -- To check if the top of the expression is a multiplication
       -- Just (_,_,Mul args) -> Just args
       _ -> Nothing

-- | Consider we know the general structure of an expression, and want to find
-- the "holes". For example, consider we know the expression is (_*x1)+(_*x2)
-- and want to find the node ids of each hole _, we can construct a pattern
-- using @phole@ to declare holes with @Capture@ ids and @pvariable@ or
-- @pconstant@ to construct pattern variables constants, then use the @match@
-- function too attempt to match a function to an expression. For example
-- @
-- -- >>> exampleMatch exampleExpr
-- @
-- This will successfully match (return @Just m@ and not @Nothing@) which will
-- contain a map from @Capture@ ids (i.e. holes) to the @NodeID@s they matched
-- with
exampleMatch :: IsExpression e => e -> Map Capture NodeID
exampleMatch expr =
  let
    (exprMap,nID) = asRawExpr expr
    h1 = phole 1
    h2 = phole 2
    x1 = pvariable "x1"
    x2 = pvariable "x2"
    pat = (h1 * x1) + (h2 * x2)
  in case match (exprMap,nID) pat of
       Just (Match captures _ _ _) -> captures
       Nothing -> error $ "given expression does not match pattern (_*x1)+(_*x2)"

-- | Consider you want to match a pattern that occurs somewhere in an expression
-- (perhaps in multiple places) but you don't know the exact NodeID's where. You
-- can use the @matchAll@ expression to do so
exampleMatchAll :: IsExpression e => e -> [Match]
exampleMatchAll expr =
  let
    (exprMap,nID) = asRawExpr expr
    h1 = phole 1
    x1 = pvariable "x1"
    pat = (h1 * x1)
  in matchAll (exprMap,nID) pat

-- | Expressions can be simplified using the "simplify" function, as easy as that!
-- NOTE the expression @2*2@ will not be recognized as a constant until
-- simplified
-- NOTE^2 when using constructProblem on a OptimizationProblem,
-- simplify is automatically called on all constraints, gradients and the objective
-- Try calling simplify on some of the example expressions
exampleSimplify :: (Maybe Double,Maybe Double)
exampleSimplify =
  let
    f :: TypedExpr Scalar R
    f = 2*2

    fRaw :: RawExpr
    fRaw = asRawExpr f
  in (exprIsConstant fRaw
     ,exprIsConstant $ simplify $ fRaw)

-- | Hashed Expression supports calculation of partial derivatives using reverse
-- symbolic differentiation, via the function @partialDerivativesMap@ See
-- <HashedExpression/Differentiation/Reverse.hs>
-- The function returns a single Expression Map containing all the partial
-- derivatives, and a Map from variable labels to the @NodeID@ containing the
-- partial derivative in the Expression Map
exampleDerivatives :: IsScalarReal e => e -> String
exampleDerivatives expr =
  let
    (exprMap,derivativeMap) = partialDerivativesMap expr
  in case Map.lookup "x1" derivativeMap of
       -- NOTE you can use prettify to turn any RawExpr into a nicely
       -- viewable String
       Just nodeID -> prettify $ simplify (exprMap,nodeID)
       Nothing -> error "expression did not contain a variable x1 to find the partial derivative of"

-- | An interesting way to check if an expression is linear is by checking its
-- partial derivatives. All the partial derivatives of a linear function are
-- constants (when fully simplified)
exprIsLinear :: IsScalarReal e => e -> Bool
exprIsLinear expr =
  let
    (exprMap,derivativeMap) = partialDerivativesMap expr
    derivativeNodeIDs = Map.elems derivativeMap
     -- expression is linear if forall p is a partial derivative, p is constant
  in and $ map (\nID ->
                  isJust $ exprIsConstant $ simplify (exprMap,nID)) derivativeNodeIDs
