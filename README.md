# HashedExpression & Symphony   [![CircleCI](https://circleci.com/gh/dalvescb/HashedExpression/tree/master.svg?style=svg)](https://circleci.com/gh/dalvescb/HashedExpression/tree/master)

## Quick code walk through
- Main:
    - **HashedExpression.hs**: Base types (Expression, Node),
    typeclasses for constraints and operations definition.
    - **HashedOperation.hs**: All the functions for constructing expressions, including
creating vars and implementation of operations.
    - **HashedInner.hs**: All the functions dealing with internal structures of
expressions, as well as transforming expressions. This can be thought as the core
of HashedExpression.
    - **HashedPattern.hs**: For matching and substituting on expression trees.
This for hashed expressions is like what regular expressions is for strings.
    - **HashedNormalize.hs**: For normalizing expressions, i.e, bring expressions to their normal form. Contain rules and function `normalize`:
    ```haskell
    normalize :: (DimensionType d, ElementType et) => Expression d et -> Expression d et
    ```
    - **HashedDerivative.hs**: For computing derivative of real expressions:
    ```haskell
    exteriorDerivative ::
       (DimensionType d)
    => Set String -- ^ Variables
    -> Expression d R  -- ^ Expression
    -> Expression d Covector
    ```
    e.g:
    ```haskell
    exteriorDerivative {x, y} (x^3 + y^3 + 2xy) = 3x^2 * dx + 3y^2 * dy + 2x * dy + 2y * dx
    ```
    - **HashedCollect.hs**: For collecting and grouping partial derivatives w.r.t each variable
    after taking derivative.
    ```haskell
    collectDifferentials :: Expression Scalar Covector -> Expression Scalar Covector
    ```
    e.g:
    ```haskell
    collectDifferentials (3x^2 * dx + 3y^2 * dy + 2x * dy + 2y * dx) = (3x^2 + 2y) * dx + (3y^2 + 2x) * dy
    ```
    - **HashedInterp.hs**: Haskell interpreter to evaluate expressions, useful for testing.
    - **HashedToC.hs**: For generating C code to evaluate expression.
    - **HashedSolver.hs**: Construct optimization problems (out of objective expressions, constraint expressions, etc.)
    process, schedule and generate C code: evaluate partial derivatives, objective, jacobian. Optimization solvers then use
    it to solve optimization problem.

- Helpers:
    - **HashedHash.hs**: All the stuffs related to hashing.
    - **HashedNode.hs**: Functions to get information of expression nodes.
    - **HashedPrettify.hs**: Printing expressions, useful for debugging.
    - **HashedPlot.hs**: For plotting expressions, useful for debugging.
    - **HashedVar.hs** and **HashedUtils.hs** : Utilities


## Style Guide
- hindent
    - Install: `stack install hindent`
    - `hindent --indent-size 4 $FILE_PATH$`    

## Check for non-exhaustive patterns:
- `stack clean`
- `stack build --fast --ghc-options -Wincomplete-patterns`
