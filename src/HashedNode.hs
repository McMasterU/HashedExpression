module HashedNode where

import HashedExpression

-- | Helpers functions for Expression nodes
--
nodeElementType :: Node -> ET
nodeElementType node =
    case node of
        Var _ -> R
        DVar _ -> Covector
        Const _ -> R
        Sum et _ -> et
        Mul et _ -> et
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


-- | For HashedPattern
--
sameOp :: Node -> Node -> Bool
sameOp node1 node2 =
    case (node1, node2) of
        (Sum _ _, Sum _ _) -> True
        (Mul _ _, Mul _ _) -> True
        (Div _ _, Div _ _) -> True
        (Sqrt _, Sqrt _) -> True
        (Sin _, Sin _) -> True
        (Cos _, Cos _) -> True
        (Tan _, Tan _) -> True
        (Exp _, Exp _) -> True
        (Log _, Log _) -> True
        (Sinh _, Sinh _) -> True
        (Cosh _, Cosh _) -> True
        (Tanh _, Tanh _) -> True
        (Asin _, Asin _) -> True
        (Acos _, Acos _) -> True
        (Atan _, Atan _) -> True
        (Asinh _, Asinh _) -> True
        (Acosh _, Acosh _) -> True
        (Atanh _, Atanh _) -> True
        (RealImag _ _, RealImag _ _) -> True
        (RealPart _, RealPart _) -> True
        (ImagPart _, ImagPart _) -> True
        _ -> False

args :: Node -> Args
args node =
    case node of
        Var _ -> []
        DVar _ -> []
        Const _ -> []
        Sum et args -> args
        Mul et args -> args
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
