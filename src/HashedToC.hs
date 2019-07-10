{-# LANGUAGE TupleSections #-}

module HashedToC where

import Data.Graph (buildG, topSort)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', intercalate, intersperse, tails)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set, empty, insert, member)
import qualified Data.Set as Set
import HashedExpression
    ( DimensionType
    , ET(..)
    , Expression(..)
    , ExpressionMap
    , Node(..)
    , NumType
    , Shape
    )
import HashedHash
import HashedInner
import HashedNode
import HashedUtils

-- | Topological sort the expression map, all the dependencies will appear before the depended node
--
topologicalSort :: (ExpressionMap, Int) -> [Int]
topologicalSort expr@(mp, n) =
    reverse . mapMaybe (`IM.lookup` vertices2nId) $ topSort graph
  where
    nId2vertices = IM.fromList $ zip (IM.keys mp) [0 ..]
    vertices2nId = IM.fromList $ zip [0 ..] (IM.keys mp)
    exNodeEdges = expressionEdges expr
    verticesEdges =
        mapMaybe
            (bringMaybeOut . mapBoth (`IM.lookup` nId2vertices))
            exNodeEdges
    graph = buildG (0, IM.size mp - 1) verticesEdges

-- | Get all the edges of the expressions
--
expressionEdges :: (ExpressionMap, Int) -> [(Int, Int)]
expressionEdges (mp, n) = Set.toList $ edges n
  where
    edges :: Int -> Set (Int, Int)
    edges nId =
        let args = nodeArgs $ retrieveNode nId mp
            thisNode = Set.fromList . map (nId, ) $ args
         in Set.unions $ thisNode : map edges args

-- | Mem map (offset, R or C, shape)
--
type MemMapEntry = (Int, EntryType, Shape)

data MemMap =
    MemMap
        { entryMap :: IntMap MemMapEntry -- node id -> (offset, size)
        , totalSize :: Int
        }
    deriving (Show, Eq, Ord)

data EntryType
    = EntryR
    | EntryC
    deriving (Show, Eq, Ord)

data PartC
    = Re
    | Im
    deriving (Show, Eq, Ord)

data GridIndex
    = GridR [Int]
    | GridC PartC [Int]
    deriving (Show, Eq, Ord)

-- |
--
sizeDouble = 8

-- | Make a memory map from an expression
--
makeMemMap :: (DimensionType d, NumType et) => Expression d et -> MemMap
makeMemMap expr@(Expression n mp) = uncurry MemMap $ foldl' f (IM.empty, 0) nIds
  where
    nIds = IM.keys mp
    f :: (IntMap MemMapEntry, Int) -> Int -> (IntMap MemMapEntry, Int)
    f (memMapSoFar, sizeSoFar) nId =
        let (shape, node) = retrieveInternal nId mp
            (nodeSz, mmShape)
                | nodeElementType node mp == R =
                    (product shape * sizeDouble, EntryR)
                | nodeElementType node mp == C =
                    (2 * product shape * sizeDouble, EntryC)
            newMemMap = IM.insert nId (sizeSoFar, mmShape, shape) memMapSoFar
         in (newMemMap, sizeSoFar + nodeSz)

-- |
--
memOffset :: MemMap -> Int -> GridIndex -> Int
memOffset (MemMap entryMap _) nId gridIndex
    | Just (globalOffset, entryType, entryShape) <- IM.lookup nId entryMap =
        case (entryType, gridIndex) of
            (EntryR, GridR indices) ->
                globalOffset + localOffset entryShape indices
            (EntryC, GridC Re indices) ->
                globalOffset + localOffset (2 : entryShape) (0 : indices)
            (EntryC, GridC Im indices) ->
                globalOffset + localOffset (2 : entryShape) (1 : indices)
    | otherwise = error "node id is not in the mem map"

-- | Compute the inner offset:
-- e.g localOffset [3, 4, 5] [2, 0, 1] = 2 * (4 * 5) + 0 * (5) + 1
--
localOffset :: [Int] -> [Int] -> Int
localOffset shape indices
    | length shape == length indices =
        (* sizeDouble) . sum . zipWith (*) indices . map product . tail . tails $
        shape
    | otherwise =
        error $
        "shape and indices are not compatible" ++ show shape ++ show indices

-- | Assigning
--
infixl 1 <<-, +=

(<<-) :: String -> String -> String
(<<-) a b = a ++ " = " ++ b ++ ";"

(+=) :: String -> String -> String
(+=) a b = a ++ " += " ++ b ++ ";"

space :: Int -> [String] -> [String]
space n = map (replicate n ' ' ++)

[i, j, k, noOffset] = ["i", "j", "k", "0"]

-- |
--
generateCodeC ::
       (DimensionType d, NumType et) => MemMap -> Expression d et -> [String]
generateCodeC memMap expr = concatMap genCode . topologicalSort . unwrap $ expr
  where
    mp = fst . unwrap $ expr
    for :: String -> Int -> [String] -> [String]
    for iter nId scopeCodes = forWith [] iter nId scopeCodes []
    forWith :: [String] -> String -> Int -> [String] -> [String] -> [String]
    forWith initCodes iter nId scopeCodes afterCode =
        let shape = retrieveShape nId mp
         in ["{"] ++
            space 2 initCodes ++
            [ "  int " ++ iter ++ ";"
            , "  for (" ++
              iter ++
              " = 0; " ++
              iter ++
              " < 8 * " ++ show (product shape) ++ "; " ++ iter ++ " += 8) {"
            ] ++
            space 4 scopeCodes ++
            ["  }"] ++ --
            space 2 afterCode ++ --
            ["}"]
    -- Real node
    at :: Int -> String -> String
    at nId offsetVar =
        let nShape = retrieveShape nId mp
            gridIndex = GridR $ replicate (length nShape) 0
            offsetValue
                | offsetVar == "0" = ""
                | otherwise = " + " ++ offsetVar
         in "(*((double*)(ptr + " ++
            show (memOffset memMap nId gridIndex) ++ offsetValue ++ ")))"
    -- Real part of complex node
    reAt :: Int -> String -> String
    reAt nId offsetVar =
        let nShape = retrieveShape nId mp
            gridIndex = GridC Re $ replicate (length nShape) 0
            offsetValue
                | offsetVar == "0" = ""
                | otherwise = "+ " ++ offsetVar
         in "(*((double*)(ptr + " ++
            show (memOffset memMap nId gridIndex) ++ offsetValue ++ ")))"
    -- Real part of complex node
    imAt :: Int -> String -> String
    imAt nId offsetVar =
        let nShape = retrieveShape nId mp
            gridIndex = GridC Im $ replicate (length nShape) 0
            offsetValue
                | offsetVar == "0" = ""
                | otherwise = "+ " ++ offsetVar
         in "(*((double*)(ptr + " ++
            show (memOffset memMap nId gridIndex) ++ offsetValue ++ ")))"
    infix 9 `at`, `imAt`, `reAt`
    computedValAt :: Int -> String -> String
    computedValAt nId offsetVar
        | Const val <- retrieveNode nId mp = "(" ++ show val ++ ")"
        | otherwise =
            let gridIndex = GridR []
             in "(*((double*)(ptr + " ++
                show (memOffset memMap nId gridIndex) ++ offsetVar ++ ")))"
    genCode :: Int -> [String] -- From node id to codes
    genCode n =
        let (shape, node) = retrieveInternal n mp
            elementType nId = retrieveElementType nId mp
         in case node of
                Var _ -> []
                DVar _ -> error "DVar should not be here"
                Const _ -> [] -- TODO: Do we need to assign to const ???
                Sum _ args
                    | elementType n == R ->
                        let sumAt i = intercalate " + " $ map (`at` i) args
                         in for i n [n `at` i <<- sumAt i]
                    | elementType n == C ->
                        let sumReAt i = intercalate " + " $ map (`reAt` i) args
                            sumImAt i = intercalate " + " $ map (`imAt` i) args
                         in for i n [n `reAt` i <<- sumReAt i] ++
                            for i n [n `imAt` i <<- sumImAt i]
                Mul _ args
                    | elementType n == R ->
                        let prodAt i = intercalate " * " $ map (`at` i) args
                         in for i n [n `at` i <<- prodAt i]
                    | otherwise -> error "Not support yet"
                Power x arg
                    | elementType n == R ->
                        let powerAt i = arg `at` i ++ "^ (" ++ show x ++ ")"
                         in for i n [n `at` i <<- powerAt i]
                    | otherwise -> error "Not support yet"
                Neg _ arg
                    | elementType n == R ->
                        let negAt i = "-" ++ arg `at` i
                         in for i n [n `at` i <<- negAt i]
                    | elementType n == C ->
                        let negReAt i = "-" ++ (arg `reAt` i)
                            negImAt i = "-" ++ (arg `imAt` i)
                         in for i n [n `reAt` i <<- negReAt i] ++
                            for i n [n `imAt` i <<- negImAt i]
                Scale _ scalar arg
                    | (elementType scalar, elementType arg) == (R, R) ->
                        let scaleAt i =
                                (scalar `at` noOffset) ++ "*" ++ arg `at` i
                         in for i n [n `at` i <<- scaleAt i]
                    | (elementType scalar, elementType arg) == (R, C) ->
                        let scaleRe i =
                                (scalar `at` noOffset) ++ "*" ++ (arg `reAt` i)
                            scaleIm i =
                                (scalar `at` noOffset) ++ "*" ++ (arg `imAt` i)
                         in for i n [n `reAt` i <<- scaleRe i] ++
                            for i n [n `reAt` i <<- scaleIm i]
                    | otherwise -> error "Not support yet"
                -- MARK: only apply to R
                Div arg1 arg2 ->
                    let divAt i = arg1 `at` i ++ " / " ++ arg2 `at` i
                     in for i n [n `at` i <<- divAt i]
                Sqrt arg -> for i n [n `at` i <<- "sqrt" ++ arg `at` i]
                Sin arg -> for i n [n `at` i <<- "sin" ++ arg `at` i]
                Cos arg -> for i n [n `at` i <<- "cos" ++ arg `at` i]
                Tan arg -> for i n [n `at` i <<- "tan" ++ arg `at` i]
                Exp arg -> for i n [n `at` i <<- "exp" ++ arg `at` i]
                Log arg -> for i n [n `at` i <<- "log" ++ arg `at` i]
                Sinh arg -> for i n [n `at` i <<- "sinh" ++ arg `at` i]
                Cosh arg -> for i n [n `at` i <<- "cosh" ++ arg `at` i]
                Tanh arg -> for i n [n `at` i <<- "tanh" ++ arg `at` i]
                Asin arg -> for i n [n `at` i <<- "asin" ++ arg `at` i]
                Acos arg -> for i n [n `at` i <<- "acos" ++ arg `at` i]
                Atan arg -> for i n [n `at` i <<- "atan" ++ arg `at` i]
                Asinh arg -> for i n [n `at` i <<- "asinh" ++ arg `at` i]
                Acosh arg -> for i n [n `at` i <<- "acosh" ++ arg `at` i]
                Atanh arg -> for i n [n `at` i <<- "atanh" ++ arg `at` i]
                -- MARK: Complex related
                RealImag arg1 arg2 ->
                    for i n [n `reAt` i <<- arg1 `at` i] ++
                    for i n [n `reAt` i <<- arg2 `at` i]
                RealPart arg -> for i n [n `at` i <<- arg `reAt` i]
                ImagPart arg -> for i n [n `at` i <<- arg `imAt` i]
                InnerProd _ arg1 arg2
                    | map elementType [arg1, arg2] == [R, R] ->
                        let initCodes = ["double acc" <<- "0"]
                            codes =
                                ["acc" += arg1 `at` i ++ " * " ++ arg2 `at` i]
                            afterCodes = [n `at` noOffset <<- "acc"]
                         in forWith initCodes i arg1 codes afterCodes
                    | otherwise -> error "Not support yet"

-- |
--
generateProgram ::
       (DimensionType d, NumType et) => ValMaps -> Expression d et -> [String]
generateProgram (ValMaps vm0 vm1 vm2 vm3) expr =
    [ "#include <math.h>" --
    , "#include <stdio.h>"
    , "#include <stdlib.h>"
    , "int main(){" --
    ] ++
    space 2 (initMemory ++ assignVals ++ codes ++ printValue ++ releaseMemory) ++
    [ "}" --
    ]
  where
    infix 9 `at`
    at :: Int -> String -> String
    at nId offsetVar =
        let nShape = retrieveShape nId mp
            gridIndex = GridR $ replicate (length nShape) 0
            offsetValue
                | offsetVar == "0" = ""
                | otherwise = " + " ++ offsetVar
         in "(*((double*)(ptr + " ++
            show (memOffset memMap nId gridIndex) ++ offsetValue ++ ")))"
    (mp, n) = unwrap expr
    memMap = makeMemMap expr
    -- allocate memory
    sz = totalSize memMap
    initMemory = ["void *ptr" <<- "malloc(" ++ show sz ++ ")"]
    -- assign value to variables
    vars :: [(Int, String)]
    vars =
        let toVar nId
                | Var varName <- retrieveNode nId mp = Just (nId, varName)
                | otherwise = Nothing
         in mapMaybe toVar . IM.keys $ mp
    codesForVar :: (Int, String) -> [String]
    codesForVar (n, varName)
        | Just val <- Map.lookup varName vm0 =
            [n `at` noOffset <<- show val]
        | Just val <- Map.lookup varName vm1 = []
        | Just val <- Map.lookup varName vm2 = []
        | Just val <- Map.lookup varName vm3 = []
    assignVals = []
    -- codes to compute
    codes = generateCodeC memMap expr
    -- print the value of expression
    printValue = []
    releaseMemory = ["free(ptr);"]
