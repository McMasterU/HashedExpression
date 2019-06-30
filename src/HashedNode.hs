module HashedNode where

import Data.IntMap.Strict as IM
import HashedExpression

-- | Helpers functions for Expression nodes
--
nodeElementType :: Node -> ExpressionMap -> ET
nodeElementType node mp =
    case node of
        Var _ -> R
        DVar _ -> Covector
        Const _ -> R
        Sum et _ -> et
        Mul et _ -> et
        Neg _ _ -> R
        Scale et _ _ -> et
        Div _ _ -> R
        Sqrt _ -> R
        Sin _ -> R
        Cos _ -> R
        Tan _ -> R
        Exp _ -> R
        Log _ -> R
        Sinh _ -> R
        Cosh _ -> R
        Tanh _ -> R
        Asin _ -> R
        Acos _ -> R
        Atan _ -> R
        Asinh _ -> R
        Acosh _ -> R
        Atanh _ -> R
        RealImag _ _ -> C -- from real and imagine
        RealPart _ -> R -- extract real part
        ImagPart _ -> R -- extract imaginary part
        InnerProd et _ _ -> et
        Piecewise _ _ branches -> retrieveElementType (head branches) mp

---- |
----
--sameOp :: Node -> Node -> Bool
--sameOp node1 node2 =
--    case (node1, node2) of
--        (Sum {}, Sum {}) -> True
--        (Mul {}, Mul {}) -> True
--        (Neg {}, Neg {}) -> True
--        (Scale {}, Scale {}) -> True
--        (Div {}, Div {}) -> True
--        (Sqrt {}, Sqrt {}) -> True
--        (Sin {}, Sin {}) -> True
--        (Cos {}, Cos {}) -> True
--        (Tan {}, Tan {}) -> True
--        (Exp {}, Exp {}) -> True
--        (Log {}, Log {}) -> True
--        (Sinh {}, Sinh {}) -> True
--        (Cosh {}, Cosh {}) -> True
--        (Tanh {}, Tanh {}) -> True
--        (Asin {}, Asin {}) -> True
--        (Acos {}, Acos {}) -> True
--        (Atan {}, Atan {}) -> True
--        (Asinh {}, Asinh {}) -> True
--        (Acosh {}, Acosh {}) -> True
--        (Atanh {}, Atanh {}) -> True
--        (RealImag {}, RealImag {}) -> True
--        (RealPart {}, RealPart {}) -> True
--        (ImagPart {}, ImagPart {}) -> True
--        (InnerProd {}, InnerProd {}) -> True
--        _ -> False
-- | Get list of arguments of this node
--
nodeArgs :: Node -> Args
nodeArgs node =
    case node of
        Var _ -> []
        DVar _ -> []
        Const _ -> []
        Sum _ args -> args
        Mul _ args -> args
        Neg _ arg -> [arg]
        Scale _ arg1 arg2 -> [arg1, arg2]
        Div arg1 arg2 -> [arg1, arg2]
        Sqrt arg -> [arg]
        Sin arg -> [arg]
        Cos arg -> [arg]
        Tan arg -> [arg]
        Exp arg -> [arg]
        Log arg -> [arg]
        Sinh arg -> [arg]
        Cosh arg -> [arg]
        Tanh arg -> [arg]
        Asin arg -> [arg]
        Acos arg -> [arg]
        Atan arg -> [arg]
        Asinh arg -> [arg]
        Acosh arg -> [arg]
        Atanh arg -> [arg]
        RealImag arg1 arg2 -> [arg1, arg2]
        RealPart arg -> [arg]
        ImagPart arg -> [arg]
        InnerProd _ arg1 arg2 -> [arg1, arg2]
        Piecewise _ conditionArg branches -> conditionArg : branches

-- | Auxiliary functions for operations
--
retrieveNode :: Int -> ExpressionMap -> Node
retrieveNode n mp =
    case IM.lookup n mp of
        Just (_, node) -> node
        _ -> error "node not in map"

retrieveInternal :: Int -> ExpressionMap -> Internal
retrieveInternal n mp =
    case IM.lookup n mp of
        Just internal -> internal
        _ -> error "node not in map"

retrieveElementType :: Int -> ExpressionMap -> ET
retrieveElementType n mp =
    case IM.lookup n mp of
        Just (_, node) -> nodeElementType node mp
        _ -> error "expression not in map"

retrieveShape :: Int -> ExpressionMap -> Shape
retrieveShape n mp =
    case IM.lookup n mp of
        Just (dim, _) -> dim
        _ -> error "expression not in map"

expressionElementType :: Expression d et -> ET
expressionElementType (Expression n mp) =
    case IM.lookup n mp of
        Just (_, node) -> nodeElementType node mp
        _ -> error "expression not in map"

expressionShape :: Expression d et -> Shape
expressionShape (Expression n mp) =
    case IM.lookup n mp of
        Just (dim, _) -> dim
        _ -> error "expression not in map"

expressionInternal :: Expression d et -> Internal
expressionInternal (Expression n mp) =
    case IM.lookup n mp of
        Just internal -> internal
        _ -> error "expression not in map"

expressionNode :: Expression d et -> Node
expressionNode (Expression n mp) =
    case IM.lookup n mp of
        Just (_, node) -> node
        _ -> error "expression not in map"
