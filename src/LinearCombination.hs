{-
-}
module LinearCombination where

import qualified Data.List as L
import qualified Data.Map as Map

data LinearCombination a =
    LC a (Map.Map String a)
    deriving (Eq, Ord)

instance (Show a, Eq a, Num a, Ord a) => Show (LinearCombination a) where
    show (LC c combos) =
        case (c, null nonConst) of
            (0, True) -> "0"
            (0, _) -> drop 3 nonConst
            _ -> show c ++ " + " ++ nonConst
      where
        nonConst = concatMap dispPair $ Map.toAscList combos
        showNeg c =
            if c < 0
                then "(" ++ show c ++ ")"
                else show c
        dispPair (_, 0) = ""
        dispPair (name, 1) = " + " ++ name
        dispPair (name, c) = " + " ++ showNeg c ++ " * " ++ name

instance (Num a, Show a, Eq a, Ord a) => Num (LinearCombination a) where
    (LC c combos) + (LC c' combos') =
        LC (c + c') (Map.unionWith (+) combos combos')
    negate (LC c combos) = LC (negate c) (Map.map negate combos)
    fromInteger x = LC (fromInteger x) Map.empty
    (LC c combos) * (LC c' combos')
        | Map.null combos = LC (c * c') (Map.map (* c) combos')
        | Map.null combos' = LC (c * c') (Map.map (* c') combos)
        | True =
            error $
            "can't form " ++ show (LC c combos) ++ " * " ++ show (LC c' combos')
    abs x = error $ "can't abs linear combination " ++ show x
    signum x = error $ "can't signum linear combination " ++ show x

lcVar name = LC 0 (Map.singleton name 1)

data PositiveCombination a =
    PC (Map.Map [String] a)
    deriving (Eq, Ord)

instance (Show a, Eq a, Num a, Ord a) => Show (PositiveCombination a) where
    show (PC combos) =
        if null nonZero
            then "0"
            else nonZero
      where
        nonZero =
            concat $
            L.intersperse " + " $ concatMap dispPair $ Map.toAscList combos
        dispPair (_, 0) = []
        dispPair (names, 1) = [showNames names]
        dispPair (names, c) = [showNames (showNeg c : names)]
        showNeg c =
            if c < 0
                then "(" ++ show c ++ ")"
                else show c
        showNames names = concat $ L.intersperse " * " names

-- test pcVar "x" + pcVar "y" * 3 + pcVar "x" * (pcVar "w" - 1)
instance (Num a, Show a, Eq a, Ord a) => Num (PositiveCombination a) where
    (PC combos) + (PC combos') =
        PC (Map.filter (/= 0) $ Map.unionWith (+) combos combos')
    negate (PC combos) = PC (Map.map negate combos)
    fromInteger x = PC $ Map.singleton [] (fromInteger x)
    (PC combos) * (PC combos') =
        PC $
        Map.filter (/= 0) $
        Map.fromListWith (+) $
        filter
            ((/= 0) . snd)
            [ (L.sort $ a ++ b, ac * bc)
            | (a, ac) <- Map.toAscList combos
            , (b, bc) <- Map.toAscList combos'
            ]
    abs x = error $ "can't abs linear combination " ++ show x
    signum x = error $ "can't signum linear combination " ++ show x

pcVar name = PC (Map.singleton [name] 1)
{-
-}
