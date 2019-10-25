-- Haskell data types for the abstract syntax.
-- Generated by the BNF converter.

module AbsHashedLang where

newtype KWDataPattern = KWDataPattern String
  deriving (Eq, Ord, Show, Read)

newtype PIdent = PIdent ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

data Problem = Problem [Block]
  deriving (Eq, Ord, Show, Read)

data Block
    = BlockVariable [[VariableDecl]]
    | BlockConstant [[ConstantDecl]]
    | BlockLet [[LetDecl]]
    | BlockMinimize Exp
  deriving (Eq, Ord, Show, Read)

data Number = NumInt Integer | NumDouble Double
  deriving (Eq, Ord, Show, Read)

data Val
    = ValFile String
    | ValDataset String String
    | ValPattern KWDataPattern
    | ValRandom
    | ValLiteral Number
  deriving (Eq, Ord, Show, Read)

data Dim = Dim Integer
  deriving (Eq, Ord, Show, Read)

data Shape
    = ShapeScalar | Shape1D Dim | Shape2D Dim Dim | Shape3D Dim Dim Dim
  deriving (Eq, Ord, Show, Read)

data VariableDecl
    = VariableNoInit PIdent Shape | VariableWithInit PIdent Shape Val
  deriving (Eq, Ord, Show, Read)

data ConstantDecl = ConstantDecl PIdent Shape Val
  deriving (Eq, Ord, Show, Read)

data LetDecl = LetDecl PIdent Exp
  deriving (Eq, Ord, Show, Read)

data RotateAmount
    = RA1D Integer
    | RA2D Integer Integer
    | RA3D Integer Integer Integer
  deriving (Eq, Ord, Show, Read)

data Exp
    = EPlus Exp Exp
    | ESubtract Exp Exp
    | EMul Exp Exp
    | EDiv Exp Exp
    | EScale Exp Exp
    | EDot Exp Exp
    | EFun PIdent Exp
    | ERotate RotateAmount Exp
    | ENum Number
    | EIdent PIdent
  deriving (Eq, Ord, Show, Read)

