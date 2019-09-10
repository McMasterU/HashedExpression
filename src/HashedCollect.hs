module HashedCollect where

import Data.Function.HT (nest)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
    ( elemIndex
    , foldl'
    , group
    , groupBy
    , intercalate
    , partition
    , sort
    )
import Data.List.NonEmpty (groupWith)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Debug.Trace (traceShow, traceShowId)
import GHC.Exts (sortWith)
import HashedExpression
import HashedHash
import HashedInner
import HashedNode
import HashedOperation (const, const1d, const2d, const3d)
import HashedPattern
import HashedPrettify
import HashedSimplify
import HashedUtils
import Prelude hiding
    ( (*)
    , (+)
    , (-)
    , (/)
    , (^)
    , acos
    , acosh
    , asin
    , asinh
    , atan
    , atanh
    , const
    , const
    , cos
    , cosh
    , exp
    , log
    , negate
    , product
    , sin
    , sinh
    , sqrt
    , sum
    , tan
    , tanh
    )
import qualified Prelude

-- | Precondition: (satisfied by first applying simplification)
-- - No complex in the input (:+, xRe, xIm)
-- - Scale is pushed to the outer most layer and real scalars are group together in a product
--
collectDifferentials :: Expression Zero Covector -> Expression Zero Covector
collectDifferentials = wrap . applyRules . unwrap . simplify
  where
    applyRules =
        chain
            [ restructure
            , toRecursiveCollecting splitCovectorProdRules
            , separateDVarAlone
            , toTransformation groupByDVar
            , aggregateByDVar
            , simplifyEachPartialDerivative
            , removeUnreachable
            ]

inspect :: Transformation
inspect exp = traceShow (debugPrint exp) exp

-- |
--
toRecursiveCollecting :: Modification -> Transformation
toRecursiveCollecting = toTransformation . toRecursiveModification NoReorder

-- | Change to multiplication whenever possible, then flatten sum and product to prepare for splitCovectorProdRules
-- Also move covector to the right hand side of dot product
--
restructure :: Transformation
restructure =
    multipleTimes 1000 $
    chain
        [ toMultiplyIfPossible --
        , toRecursiveCollecting flattenSumProdRules
        ]

-- | x * y * covector * z --> (x * y * z) * covector
--
splitCovectorProdRules :: Modification
splitCovectorProdRules exp@(mp, n) =
    case retrieveNode n mp of
        Mul Covector ns ->
            let (covectorPart, realPart) =
                    partition ((== Covector) . flip retrieveElementType mp) ns
                prodRealPart = mulManyDiff mp . map noChange $ realPart
             in mulManyDiff mp $ prodRealPart : map noChange covectorPart
        _ -> noChange n

-- |
--
separateDVarAlonePiecewise :: Modification
separateDVarAlonePiecewise (mp, n)
    | Piecewise marks condition branches <- retrieveNode n mp
    , retrieveElementType n mp == Covector
    , [mainBranch] <- filter (not . isZero mp) branches
    , Just k <- elemIndex mainBranch branches =
        let shape = retrieveShape n mp
            one = diffConst shape 1
            zero = diffConst shape 0
            numBranches = length branches
         in case retrieveNode mainBranch mp of
                DVar varName ->
                    let branchesWithZero =
                            replicate k zero ++
                            [one] ++ replicate (numBranches - k - 1) zero
                        partDiff =
                            applyDiff mp (conditionAry (Piecewise marks)) $
                            noChange condition : branchesWithZero
                     in mulManyDiff mp [partDiff, noChange mainBranch]
                Mul Covector [pd, dVarPart] ->
                    let branchesWithZero =
                            replicate k zero ++
                            [noChange pd] ++
                            replicate (numBranches - k - 1) zero
                        partDiff =
                            applyDiff mp (conditionAry (Piecewise marks)) $
                            noChange condition : branchesWithZero
                     in mulManyDiff mp [partDiff, noChange dVarPart]
                InnerProd Covector pd dVarPart ->
                    let innerShape = retrieveShape dVarPart mp
                        innerZero = diffConst innerShape 0
                        innerOne = diffConst innerShape 1
                        newCondition =
                            applyDiff
                                mp
                                (binaryET Scale ElementDefault)
                                [noChange condition, innerOne]
                        branchesWithZero =
                            replicate k innerZero ++
                            [noChange pd] ++
                            replicate (numBranches - k - 1) innerZero
                        partDiff =
                            applyDiff mp (conditionAry (Piecewise marks)) $
                            newCondition : branchesWithZero
                     in applyDiff
                            mp
                            (binaryET InnerProd ElementDefault)
                            [partDiff, noChange dVarPart]
                _ -> error $ debugPrint (mp, mainBranch)
    | otherwise = noChange n

-- |
--
separateDVarAlone :: Transformation
separateDVarAlone =
    multipleTimes 1000 . chain $
    map (toRecursiveCollecting . fromSubstitution)
        [ x <.> (restOfProduct ~* y) |. isCovector y ~~~~~~>
          ((restOfProduct ~* x) <.> y)
        , x <.> (z * y) |. isDVar y ~~~~~~> (z * x) <.> y
        , x <.> (restOfProduct ~* y) |. isDVar y ~~~~~~> (restOfProduct ~* x) <.>
          y
        , s * (x <.> y) |. isDVar y ~~~~~~> (s *. x) <.> y
        , s * (x * y) |. isDVar y ~~~~~~> (s * x) * y
        -- Dealing with rotate
        , x <.> rotate amount y |. isCovector y ~~~~~~>
          (rotate (negate amount) x <.> y)
        ] ++
    [toRecursiveCollecting separateDVarAlonePiecewise]

-- | Group a sum to many sums, each sum is corresponding to a DVar, preparing for aggregateByDVar
-- (f * dx + h * dy + dx + t1 <.> dx1 + f1 <.> dx1) -->
--   ((f * dx + 1 * dx) + (h * dy) + (t1 <.> dx1 + f1 <.> dx1)
--
groupByDVar :: Modification
groupByDVar exp@(mp, n) =
    case retrieveNode n mp of
        Sum Covector ns ->
            let groups =
                    groupBy sameDVar .
                    sortWith getDVar . filter (not . isZero mp) $
                    ns
             in sumManyDiff mp . map (sumManyDiff mp . map mulOneIfAlone) $
                groups
        _ -> mulOneIfAlone n
  where
    getDVar :: Int -> String
    getDVar nId
        | DVar name <- retrieveNode nId mp = name
        | Mul Covector [_, cId] <- retrieveNode nId mp
        , DVar name <- retrieveNode cId mp = name
        | InnerProd Covector _ cId <- retrieveNode nId mp
        , DVar name <- retrieveNode cId mp = name
        | otherwise = error $ "Collect D: " ++ debugPrint (mp, nId)
    sameDVar :: Int -> Int -> Bool
    sameDVar nId1 nId2 = getDVar nId1 == getDVar nId2
    mulOneIfAlone nId
        | DVar _ <- retrieveNode nId mp =
            mulManyDiff mp [diffConst [] 1, noChange nId]
        | otherwise = noChange nId

-- | After group Dvar to groups, we aggregate result in each group
--   ((f * dx + x * dx) + (h * dy) + (t1 <.> dx1 + f1 <.> dx1)
--   --> ((f + x) * dx) + (h * dy) + ((t1 + f1) <.> dx1)
--
aggregateByDVar :: Transformation
aggregateByDVar =
    chain . map (toRecursiveCollecting . fromSubstitution) $
    [ sum (mapL (* y) xs) |. isDVar y ~~~~~~> sum xs * y
    , sum (mapL (<.> y) xs) |. isDVar y ~~~~~~> sum xs <.> y
    ]

-- | Simplify each partial derivative
--
simplifyEachPartialDerivative :: Transformation
simplifyEachPartialDerivative exp@(mp, n)
    | Sum Covector ns <- retrieveNode n mp = sumMany $ map simplifyEach ns
    | InnerProd Covector _ _ <- retrieveNode n mp = simplifyEach n
    | otherwise = (mp, n)
  where
    simplifyEach nId
        | Mul Covector [partialDeriv, dVar] <- retrieveNode nId mp =
            mulMany
                [ simplifyingTransformation CombinePiecewise (mp, partialDeriv)
                , (mp, dVar)
                ]
        | InnerProd Covector partialDeriv dVar <- retrieveNode nId mp =
            apply
                (binaryET InnerProd ElementDefault `hasShape` [])
                [ simplifyingTransformation CombinePiecewise (mp, partialDeriv)
                , (mp, dVar)
                ]
