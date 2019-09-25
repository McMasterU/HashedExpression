{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PolyKinds #-}

module HashedToC where

import Data.Array
import Data.Graph (buildG, topSort)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', intercalate, intersperse, tails)
import Data.List.HT (splitLast)
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
import HashedPrettify (prettifyDebug)
import HashedUtils

-- | Mem map (offset, R or C, shape)
--
type MemMapEntry = (Int, EntryType, Shape)

data MemMap =
    MemMap
        { entryMap :: IntMap MemMapEntry -- node id -> (offset, R or C, shape)
        , totalDoubles :: Int
        }
    deriving (Show, Eq, Ord)

data EntryType
    = EntryR
    | EntryC
    deriving (Show, Eq, Ord)

data LookupPart
    = LookupR
    | LookupReC
    | LookupImC
    deriving (Show, Eq, Ord)

-- |
--
type CodeLine = String

type Code = [CodeLine]

-- | Make a memory map from an expression
--
makeMemMap :: ExpressionMap -> MemMap
makeMemMap mp = uncurry MemMap $ foldl' f (IM.empty, 0) nIds
  where
    nIds = IM.keys mp
    f :: (IntMap MemMapEntry, Int) -> Int -> (IntMap MemMapEntry, Int)
    f (memMapSoFar, sizeSoFar) nId =
        let (shape, node) = retrieveInternal nId mp
            (nodeSz, mmShape)
                | nodeElementType node mp == R = (product shape, EntryR)
                | nodeElementType node mp == C = (2 * product shape, EntryC)
            newMemMap = IM.insert nId (sizeSoFar, mmShape, shape) memMapSoFar
         in (newMemMap, sizeSoFar + nodeSz)

-- |
--
memOffset :: MemMap -> Int -> LookupPart -> Int
memOffset (MemMap entryMap _) nId lookupPart
    | Just (globalOffset, entryType, entryShape) <- IM.lookup nId entryMap =
        case (entryType, lookupPart) of
            (EntryR, LookupR) -> globalOffset
            (EntryC, LookupReC) -> globalOffset
            (EntryC, LookupImC) -> globalOffset + product entryShape
    | otherwise = error "node id is not in the mem map"

-- | Compute the inner offset:
-- e.g localOffset [3, 4, 5] [2, 0, 1] = 2 * (4 * 5) + 0 * (5) + 1
--
localOffset :: [Int] -> [Int] -> Int
localOffset shape indices
    | length shape == length indices =
        sum . zipWith (*) indices . map product . tail . tails $ shape
    | otherwise =
        error $
        "shape and indices are not compatible" ++ show shape ++ show indices

-- | Helpers to write more readable code generation
--
infixl 1 <<-, +=

(<<-) :: String -> String -> CodeLine
(<<-) a b = a ++ " = " ++ b ++ ";"

(+=) :: String -> String -> CodeLine
(+=) a b = a ++ " += " ++ b ++ ";"

space :: Int -> Code -> Code
space n = map (replicate n ' ' ++)

scoped :: Code -> Code
scoped codes = ["{"] ++ space 2 codes ++ ["}"]

[i, j, k, noOffset] = ["i", "j", "k", "0"]

-- | Helper functions to generate codes
--
forWith :: String -> Shape -> (Code, Code, Code) -> Code
forWith iter shape (initCodes, codes, afterCodes)
    | not $ null shape =
        ["{"] ++
        space 2 initCodes ++
        [ "  int " ++ iter ++ ";"
        , "  for (" ++
          iter ++
          " = 0; " ++
          iter ++ " < " ++ show (product shape) ++ "; " ++ iter ++ "++) {"
        ] ++
        space 4 codes ++
        ["  }"] ++ --
        space 2 afterCodes ++ --
        ["}"]
    | otherwise = initCodes ++ codes ++ afterCodes

forRange :: String -> Int -> Code -> Code
forRange iter range codes =
    ["{"] ++
    [ "  int " ++ iter ++ ";"
    , "  for (" ++
      iter ++ " = 0; " ++ iter ++ " < " ++ show range ++ "; " ++ iter ++ "++) {"
    ] ++
    space 4 codes ++
    ["  }"] ++ --
    ["}"]

if_ :: String -> Code -> Code
if_ condition codes =
    ["if (" ++ condition ++ ") {"] ++ --
    space 2 codes ++ --
    ["}"]

elseif :: String -> Code -> Code
elseif condition codes =
    ["else if (" ++ condition ++ ") {"] ++ --
    space 2 codes ++ --
    ["}"]

else_ :: Code -> Code
else_ codes =
    ["else {"] ++ --
    space 2 codes ++ --
    ["}"]

-- | Access pointer
-- TODO 1: the function signature is too long?
-- TODO 2: constant should be written directly? then we have to separate left-hand side and right-hand side access?
--
accessPtr :: MemMap -> LookupPart -> ExpressionMap -> Int -> String -> String
accessPtr memMap lookupPart mp nId offsetVal =
    let shape = retrieveShape nId mp
        updatedOffsetVal
            | null shape = ""
            | offsetVal == "0" = ""
            | otherwise = " + " ++ offsetVal
     in "(ptr[" ++
        show (memOffset memMap nId lookupPart) ++ updatedOffsetVal ++ "])"

-- | Generate evaluation code (usually an expression and its partial derivatives) given an ExpressionMap and indices of nodes to be computed
--
generateEvaluatingCodes :: MemMap -> (ExpressionMap, [Int]) -> Code
generateEvaluatingCodes memMap (mp, rootIds) =
    concatMap genCode $ topologicalSortManyRoots (mp, rootIds)
  where
    getShape :: Int -> Shape
    getShape nId = retrieveShape nId mp
    -- | 
    --
    addressOf :: Int -> String
    addressOf nId = "(ptr + " ++ show (memOffset memMap nId LookupR) ++ ")"
    -- for with only body
    for :: String -> Int -> Code -> Code
    for iter nId scopeCodes = forWith iter (getShape nId) ([], scopeCodes, [])
    -- Real node
    at :: Int -> String -> String
    at = accessPtr memMap LookupR mp
    -- Real part of complex node
    reAt :: Int -> String -> String
    reAt = accessPtr memMap LookupReC mp
    -- Real part of complex node
    imAt :: Int -> String -> String
    imAt = accessPtr memMap LookupImC mp
    infix 9 `at`, `imAt`, `reAt`
    genCode :: Int -> Code -- From node id to codes
    genCode n =
        let (shape, node) = retrieveInternal n mp
            elementType nId = retrieveElementType nId mp
         in case node of
                Var _ -> []
                DVar _ -> error "DVar should not be here"
                Const val -> for i n [n `at` i <<- show val]
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
                Power x arg
                    | elementType n == R ->
                        let powerAt i =
                                "pow(" ++ arg `at` i ++ "," ++ show x ++ ")"
                         in for i n [n `at` i <<- powerAt i]
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
                    for i n [n `imAt` i <<- arg2 `at` i]
                RealPart arg -> for i n [n `at` i <<- arg `reAt` i]
                ImagPart arg -> for i n [n `at` i <<- arg `imAt` i]
                InnerProd _ arg1 arg2
                    | map elementType [arg1, arg2] == [R, R] &&
                          null (getShape arg1) ->
                        [ n `at` noOffset <<-
                          arg1 `at` noOffset ++ "*" ++ arg2 `at` noOffset
                        ]
                    | map elementType [arg1, arg2] == [R, R] &&
                          not (null (getShape arg1)) ->
                        let initCodes = ["double acc" <<- "0"]
                            codes =
                                ["acc" += arg1 `at` i ++ " * " ++ arg2 `at` i]
                            afterCodes = [n `at` noOffset <<- "acc"]
                         in forWith
                                i
                                (getShape arg1)
                                (initCodes, codes, afterCodes)
                Piecewise marks condition branches ->
                    let m:ms = map show marks
                        (b:bs, lst) = splitLast branches
                        eachMiddle (m, b) =
                            elseif
                                (condition `at` i ++ " <= " ++ m)
                                [n `at` i <<- b `at` i]
                     in for i n $
                        if_
                            (condition `at` i ++ " <= " ++ m)
                            [n `at` i <<- b `at` i] ++ -- 
                        concatMap eachMiddle (zip ms bs) ++
                        else_ --
                            [n `at` i <<- lst `at` i]
                Rotate [amount] arg ->
                    let [size] = shape
                     in forRange i size $
                        [ "int ai" <<-
                          "(" ++
                          i ++
                          " - " ++
                          show amount ++
                          " + " ++ show size ++ " ) % " ++ show size
                        , n `at` i <<- arg `at` "ai"
                        ]
                Rotate [amount1, amount2] arg ->
                    let [size1, size2] = shape
                        toIndex i j = i ++ " * " ++ show size2 ++ " + " ++ j
                     in forRange i size1 $
                        forRange j size2 $
                        [ "int ai" <<-
                          "(" ++
                          i ++
                          " - " ++
                          show amount1 ++
                          " + " ++ show size1 ++ " ) % " ++ show size1
                        , "int aj" <<-
                          "(" ++
                          j ++
                          " - " ++
                          show amount2 ++
                          " + " ++ show size2 ++ " ) % " ++ show size2
                        , n `at` toIndex i j <<- arg `at` toIndex "ai" "aj"
                        ]
                Rotate [amount1, amount2, amount3] arg ->
                    let [size1, size2, size3] = shape
                        toIndex i j k =
                            concat
                                [ i
                                , " * "
                                , show size2
                                , " * "
                                , show size3
                                , " + "
                                , j
                                , " * "
                                , show size3
                                , " + "
                                , k
                                ]
                     in forRange i size1 $
                        forRange j size2 $
                        forRange k size3 $
                        [ "int ai" <<-
                          "(" ++
                          i ++
                          " - " ++
                          show amount1 ++
                          " + " ++ show size1 ++ " ) % " ++ show size1
                        , "int aj" <<-
                          "(" ++
                          j ++
                          " - " ++
                          show amount2 ++
                          " + " ++ show size2 ++ " ) % " ++ show size2
                        , "int ak" <<-
                          "(" ++
                          k ++
                          " - " ++
                          show amount3 ++
                          " + " ++ show size3 ++ " ) % " ++ show size3
                        , n `at` toIndex i j k <<-
                          arg `at` toIndex "ai" "aj" "ak"
                        ]
                -- ReFT(ReFT(x) can be compute in linear time, TODO - Make a new node DoubleReFT ?
                ReFT _arg
                    | ReFT innerArg <- retrieveNode _arg mp
                    , retrieveElementType innerArg mp == R ->
                        case shape of
                            [size] ->
                                let functionParameters =
                                        [ show size
                                        , addressOf innerArg
                                        , addressOf n
                                        ]
                                 in [ "re_dft_twice_1d(" ++
                                      intercalate ", " functionParameters ++
                                      ");"
                                    ]
                            [size1, size2] ->
                                let functionParameters =
                                        [ show size1
                                        , show size2
                                        , addressOf innerArg
                                        , addressOf n
                                        ]
                                 in [ "re_dft_twice_2d(" ++
                                      intercalate ", " functionParameters ++
                                      ");"
                                    ]
                -- ImFT(ImFT(x) can be compute in linear time, TODO - Make a new node DoubleImFT
                ImFT _arg
                    | ImFT innerArg <- retrieveNode _arg mp
                    , retrieveElementType innerArg mp == R ->
                        case shape of
                            [size] ->
                                let functionParameters =
                                        [ show size
                                        , addressOf innerArg
                                        , addressOf n
                                        ]
                                 in [ "im_dft_twice_1d(" ++
                                      intercalate ", " functionParameters ++
                                      ");"
                                    ]
                            [size1, size2] ->
                                let functionParameters =
                                        [ show size1
                                        , show size2
                                        , addressOf innerArg
                                        , addressOf n
                                        ]
                                 in [ "im_dft_twice_2d(" ++
                                      intercalate ", " functionParameters ++
                                      ");"
                                    ]
                ReFT arg
                    | [size] <- shape
                    , retrieveElementType arg mp == R ->
                        let functionParameters =
                                [show size, addressOf arg, addressOf n, "REAL"]
                         in [ "dft_1d(" ++
                              intercalate ", " functionParameters ++ ");"
                            ]
                    | [size1, size2] <- shape
                    , retrieveElementType arg mp == R ->
                        let functionParameters =
                                [ show size1
                                , show size2
                                , addressOf arg
                                , addressOf n
                                , "REAL"
                                ]
                         in [ "dft_2d(" ++
                              intercalate ", " functionParameters ++ ");"
                            ]
                ImFT arg
                    | [size] <- shape
                    , retrieveElementType arg mp == R ->
                        let functionParameters =
                                [show size, addressOf arg, addressOf n, "IMAG"]
                         in [ "dft_1d(" ++
                              intercalate ", " functionParameters ++ ");"
                            ]
                    | [size1, size2] <- shape
                    , retrieveElementType arg mp == R ->
                        let functionParameters =
                                [ show size1
                                , show size2
                                , addressOf arg
                                , addressOf n
                                , "IMAG"
                                ]
                         in [ "dft_2d(" ++
                              intercalate ", " functionParameters ++ ");"
                            ]
                _ -> error "Not support yet "

-- | Read $numDoubles$ doubles from $fileName$ to ptr[offset]
--
generateReadValuesCode :: String -> Int -> Int -> Code
generateReadValuesCode fileName offset numDoubles =
    scoped
        [ "FILE *fp = fopen(\"" ++ fileName ++ "\", \"r\");"
        , "int i;"
        , "for (i = 0; i < " ++ show numDoubles ++ "; i++) { "
        , "  fscanf(fp, \"%lf\", &ptr[" ++ show offset ++ " + i]);"
        , "}"
        , "fclose(fp);"
        ]

-- | Code to assign values to those in val maps
--
generateAssignValueCodes :: ValMaps -> MemMap -> ExpressionMap -> Code
generateAssignValueCodes valMaps memMap mp = concatMap codesForVar vars
  where
    at = accessPtr memMap LookupR mp
    vars :: [(Int, String)]
    vars =
        let toVar nId
                | Var varName <- retrieveNode nId mp = Just (nId, varName)
                | otherwise = Nothing
         in mapMaybe toVar . IM.keys $ mp
    codesForVar :: (Int, String) -> Code
    codesForVar (n, varName) =
        case Map.lookup varName valMaps of
            Just (VScalar val) -> [n `at` noOffset <<- show val]
            Just (V1D array1d) ->
                let assignIndex id = [n `at` show id <<- show (array1d ! id)]
                 in concatMap assignIndex $ indices array1d
            Just (V2D array2d) ->
                let shape = retrieveShape n mp
                    assignIndex (id1, id2) =
                        [ n `at` show (localOffset shape [id1, id2]) <<-
                          show (array2d ! (id1, id2))
                        ]
                 in concatMap assignIndex $ indices array2d
            Just (V3D array3d) ->
                let shape = retrieveShape n mp
                    assignIndex (id1, id2, id3) =
                        [ n `at` show (localOffset shape [id1, id2, id3]) <<-
                          show (array3d ! (id1, id2, id2))
                        ]
                 in concatMap assignIndex $ indices array3d
            _ -> []

-- | Generate a fully working C program that compute the expression and print out the result
--
singleExpressionCProgram ::
       (DimensionType d, NumType et) => ValMaps -> Expression d et -> Code
singleExpressionCProgram valMaps expr =
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
    -- for with only body
    for :: String -> Int -> Code -> Code
    for iter nId scopeCodes =
        forWith iter (retrieveShape nId mp) ([], scopeCodes, [])
    -- Real node
    at :: Int -> String -> String
    at = accessPtr memMap LookupR mp
    -- Real part of complex node
    reAt :: Int -> String -> String
    reAt = accessPtr memMap LookupReC mp
    -- Real part of complex node
    imAt :: Int -> String -> String
    imAt = accessPtr memMap LookupImC mp
    (mp, n) = unwrap expr
    (shape, et) = (expressionShape expr, expressionElementType expr)
    memMap = makeMemMap mp
    -- allocate memory
    sz = totalDoubles memMap
    initMemory =
        ["double *ptr" <<- "malloc(sizeof(double) * " ++ show sz ++ ")"]
    -- assign value to variables
    assignVals = generateAssignValueCodes valMaps memMap mp
    -- codes to compute
    codes = generateEvaluatingCodes memMap (exMap expr, [exIndex expr])
    -- print the value of expression
    printValue
        | et == R = for i n ["printf(\"%f \"," ++ n `at` i ++ ");"]
        | et == C =
            for i n ["printf(\"%f \"," ++ n `reAt` i ++ ");"] ++
            ["printf(\"\\n\");"] ++
            for i n ["printf(\"%f \"," ++ n `imAt` i ++ ");"]
    releaseMemory = ["free(ptr);"]
