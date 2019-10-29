{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParHashedLang where
import AbsHashedLang
import LexHashedLang
import ErrM
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.10

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
newtype HappyWrap34 = HappyWrap34 (String)
happyIn34 :: (String) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap34 x)
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> HappyWrap34
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
newtype HappyWrap35 = HappyWrap35 (KWDataPattern)
happyIn35 :: (KWDataPattern) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap35 x)
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> HappyWrap35
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
newtype HappyWrap36 = HappyWrap36 (TokenSub)
happyIn36 :: (TokenSub) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap36 x)
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> HappyWrap36
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
newtype HappyWrap37 = HappyWrap37 (TokenPlus)
happyIn37 :: (TokenPlus) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap37 x)
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> HappyWrap37
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
newtype HappyWrap38 = HappyWrap38 (TokenReIm)
happyIn38 :: (TokenReIm) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap38 x)
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> HappyWrap38
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
newtype HappyWrap39 = HappyWrap39 (TokenMul)
happyIn39 :: (TokenMul) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap39 x)
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> HappyWrap39
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
newtype HappyWrap40 = HappyWrap40 (TokenDiv)
happyIn40 :: (TokenDiv) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap40 x)
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> HappyWrap40
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
newtype HappyWrap41 = HappyWrap41 (TokenScale)
happyIn41 :: (TokenScale) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap41 x)
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> HappyWrap41
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
newtype HappyWrap42 = HappyWrap42 (TokenDot)
happyIn42 :: (TokenDot) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap42 x)
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> HappyWrap42
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
newtype HappyWrap43 = HappyWrap43 (TokenPower)
happyIn43 :: (TokenPower) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap43 x)
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> HappyWrap43
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
newtype HappyWrap44 = HappyWrap44 (TokenRotate)
happyIn44 :: (TokenRotate) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap44 x)
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> HappyWrap44
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
newtype HappyWrap45 = HappyWrap45 (TokenCase)
happyIn45 :: (TokenCase) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap45 x)
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> HappyWrap45
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
newtype HappyWrap46 = HappyWrap46 (PInteger)
happyIn46 :: (PInteger) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap46 x)
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> HappyWrap46
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
newtype HappyWrap47 = HappyWrap47 (PDouble)
happyIn47 :: (PDouble) -> (HappyAbsSyn )
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap47 x)
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> HappyWrap47
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
newtype HappyWrap48 = HappyWrap48 (PIdent)
happyIn48 :: (PIdent) -> (HappyAbsSyn )
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap48 x)
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> HappyWrap48
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
newtype HappyWrap49 = HappyWrap49 (Problem)
happyIn49 :: (Problem) -> (HappyAbsSyn )
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap49 x)
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> HappyWrap49
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
newtype HappyWrap50 = HappyWrap50 (Block)
happyIn50 :: (Block) -> (HappyAbsSyn )
happyIn50 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap50 x)
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> HappyWrap50
happyOut50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut50 #-}
newtype HappyWrap51 = HappyWrap51 ([Block])
happyIn51 :: ([Block]) -> (HappyAbsSyn )
happyIn51 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap51 x)
{-# INLINE happyIn51 #-}
happyOut51 :: (HappyAbsSyn ) -> HappyWrap51
happyOut51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut51 #-}
newtype HappyWrap52 = HappyWrap52 (TInt)
happyIn52 :: (TInt) -> (HappyAbsSyn )
happyIn52 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap52 x)
{-# INLINE happyIn52 #-}
happyOut52 :: (HappyAbsSyn ) -> HappyWrap52
happyOut52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut52 #-}
newtype HappyWrap53 = HappyWrap53 (TDouble)
happyIn53 :: (TDouble) -> (HappyAbsSyn )
happyIn53 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap53 x)
{-# INLINE happyIn53 #-}
happyOut53 :: (HappyAbsSyn ) -> HappyWrap53
happyOut53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut53 #-}
newtype HappyWrap54 = HappyWrap54 (Number)
happyIn54 :: (Number) -> (HappyAbsSyn )
happyIn54 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap54 x)
{-# INLINE happyIn54 #-}
happyOut54 :: (HappyAbsSyn ) -> HappyWrap54
happyOut54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut54 #-}
newtype HappyWrap55 = HappyWrap55 (Val)
happyIn55 :: (Val) -> (HappyAbsSyn )
happyIn55 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap55 x)
{-# INLINE happyIn55 #-}
happyOut55 :: (HappyAbsSyn ) -> HappyWrap55
happyOut55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut55 #-}
newtype HappyWrap56 = HappyWrap56 (Dim)
happyIn56 :: (Dim) -> (HappyAbsSyn )
happyIn56 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap56 x)
{-# INLINE happyIn56 #-}
happyOut56 :: (HappyAbsSyn ) -> HappyWrap56
happyOut56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut56 #-}
newtype HappyWrap57 = HappyWrap57 (Shape)
happyIn57 :: (Shape) -> (HappyAbsSyn )
happyIn57 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap57 x)
{-# INLINE happyIn57 #-}
happyOut57 :: (HappyAbsSyn ) -> HappyWrap57
happyOut57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut57 #-}
newtype HappyWrap58 = HappyWrap58 (VariableDecl)
happyIn58 :: (VariableDecl) -> (HappyAbsSyn )
happyIn58 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap58 x)
{-# INLINE happyIn58 #-}
happyOut58 :: (HappyAbsSyn ) -> HappyWrap58
happyOut58 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut58 #-}
newtype HappyWrap59 = HappyWrap59 ([VariableDecl])
happyIn59 :: ([VariableDecl]) -> (HappyAbsSyn )
happyIn59 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap59 x)
{-# INLINE happyIn59 #-}
happyOut59 :: (HappyAbsSyn ) -> HappyWrap59
happyOut59 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut59 #-}
newtype HappyWrap60 = HappyWrap60 ([[VariableDecl]])
happyIn60 :: ([[VariableDecl]]) -> (HappyAbsSyn )
happyIn60 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap60 x)
{-# INLINE happyIn60 #-}
happyOut60 :: (HappyAbsSyn ) -> HappyWrap60
happyOut60 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut60 #-}
newtype HappyWrap61 = HappyWrap61 (ConstantDecl)
happyIn61 :: (ConstantDecl) -> (HappyAbsSyn )
happyIn61 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap61 x)
{-# INLINE happyIn61 #-}
happyOut61 :: (HappyAbsSyn ) -> HappyWrap61
happyOut61 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut61 #-}
newtype HappyWrap62 = HappyWrap62 ([ConstantDecl])
happyIn62 :: ([ConstantDecl]) -> (HappyAbsSyn )
happyIn62 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap62 x)
{-# INLINE happyIn62 #-}
happyOut62 :: (HappyAbsSyn ) -> HappyWrap62
happyOut62 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut62 #-}
newtype HappyWrap63 = HappyWrap63 ([[ConstantDecl]])
happyIn63 :: ([[ConstantDecl]]) -> (HappyAbsSyn )
happyIn63 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap63 x)
{-# INLINE happyIn63 #-}
happyOut63 :: (HappyAbsSyn ) -> HappyWrap63
happyOut63 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut63 #-}
newtype HappyWrap64 = HappyWrap64 (LetDecl)
happyIn64 :: (LetDecl) -> (HappyAbsSyn )
happyIn64 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap64 x)
{-# INLINE happyIn64 #-}
happyOut64 :: (HappyAbsSyn ) -> HappyWrap64
happyOut64 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut64 #-}
newtype HappyWrap65 = HappyWrap65 ([LetDecl])
happyIn65 :: ([LetDecl]) -> (HappyAbsSyn )
happyIn65 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap65 x)
{-# INLINE happyIn65 #-}
happyOut65 :: (HappyAbsSyn ) -> HappyWrap65
happyOut65 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut65 #-}
newtype HappyWrap66 = HappyWrap66 ([[LetDecl]])
happyIn66 :: ([[LetDecl]]) -> (HappyAbsSyn )
happyIn66 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap66 x)
{-# INLINE happyIn66 #-}
happyOut66 :: (HappyAbsSyn ) -> HappyWrap66
happyOut66 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut66 #-}
newtype HappyWrap67 = HappyWrap67 (Bound)
happyIn67 :: (Bound) -> (HappyAbsSyn )
happyIn67 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap67 x)
{-# INLINE happyIn67 #-}
happyOut67 :: (HappyAbsSyn ) -> HappyWrap67
happyOut67 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut67 #-}
newtype HappyWrap68 = HappyWrap68 (ConstraintDecl)
happyIn68 :: (ConstraintDecl) -> (HappyAbsSyn )
happyIn68 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap68 x)
{-# INLINE happyIn68 #-}
happyOut68 :: (HappyAbsSyn ) -> HappyWrap68
happyOut68 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut68 #-}
newtype HappyWrap69 = HappyWrap69 ([ConstraintDecl])
happyIn69 :: ([ConstraintDecl]) -> (HappyAbsSyn )
happyIn69 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap69 x)
{-# INLINE happyIn69 #-}
happyOut69 :: (HappyAbsSyn ) -> HappyWrap69
happyOut69 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut69 #-}
newtype HappyWrap70 = HappyWrap70 ([[ConstraintDecl]])
happyIn70 :: ([[ConstraintDecl]]) -> (HappyAbsSyn )
happyIn70 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap70 x)
{-# INLINE happyIn70 #-}
happyOut70 :: (HappyAbsSyn ) -> HappyWrap70
happyOut70 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut70 #-}
newtype HappyWrap71 = HappyWrap71 (RotateAmount)
happyIn71 :: (RotateAmount) -> (HappyAbsSyn )
happyIn71 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap71 x)
{-# INLINE happyIn71 #-}
happyOut71 :: (HappyAbsSyn ) -> HappyWrap71
happyOut71 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut71 #-}
newtype HappyWrap72 = HappyWrap72 (PiecewiseCase)
happyIn72 :: (PiecewiseCase) -> (HappyAbsSyn )
happyIn72 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap72 x)
{-# INLINE happyIn72 #-}
happyOut72 :: (HappyAbsSyn ) -> HappyWrap72
happyOut72 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut72 #-}
newtype HappyWrap73 = HappyWrap73 ([PiecewiseCase])
happyIn73 :: ([PiecewiseCase]) -> (HappyAbsSyn )
happyIn73 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap73 x)
{-# INLINE happyIn73 #-}
happyOut73 :: (HappyAbsSyn ) -> HappyWrap73
happyOut73 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut73 #-}
newtype HappyWrap74 = HappyWrap74 (Exp)
happyIn74 :: (Exp) -> (HappyAbsSyn )
happyIn74 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap74 x)
{-# INLINE happyIn74 #-}
happyOut74 :: (HappyAbsSyn ) -> HappyWrap74
happyOut74 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut74 #-}
newtype HappyWrap75 = HappyWrap75 (Exp)
happyIn75 :: (Exp) -> (HappyAbsSyn )
happyIn75 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap75 x)
{-# INLINE happyIn75 #-}
happyOut75 :: (HappyAbsSyn ) -> HappyWrap75
happyOut75 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut75 #-}
newtype HappyWrap76 = HappyWrap76 (Exp)
happyIn76 :: (Exp) -> (HappyAbsSyn )
happyIn76 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap76 x)
{-# INLINE happyIn76 #-}
happyOut76 :: (HappyAbsSyn ) -> HappyWrap76
happyOut76 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut76 #-}
newtype HappyWrap77 = HappyWrap77 (Exp)
happyIn77 :: (Exp) -> (HappyAbsSyn )
happyIn77 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap77 x)
{-# INLINE happyIn77 #-}
happyOut77 :: (HappyAbsSyn ) -> HappyWrap77
happyOut77 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut77 #-}
newtype HappyWrap78 = HappyWrap78 (Exp)
happyIn78 :: (Exp) -> (HappyAbsSyn )
happyIn78 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap78 x)
{-# INLINE happyIn78 #-}
happyOut78 :: (HappyAbsSyn ) -> HappyWrap78
happyOut78 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut78 #-}
newtype HappyWrap79 = HappyWrap79 (Exp)
happyIn79 :: (Exp) -> (HappyAbsSyn )
happyIn79 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap79 x)
{-# INLINE happyIn79 #-}
happyOut79 :: (HappyAbsSyn ) -> HappyWrap79
happyOut79 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut79 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\xb7\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbc\x0d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe0\x6d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\xc0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x78\x00\x80\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\xe0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x40\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x02\x3e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x10\xf0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x01\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x08\xe8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x40\x40\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x02\x3a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x10\xd0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x10\xf0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x1c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x80\x80\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1a\x00\x00\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\xb7\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x10\xf0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x80\x80\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x04\x74\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x20\xa0\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x1c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\xe0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x80\x80\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x04\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x01\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x60\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x90\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x02\x3a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x10\xd0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\xe0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x01\x1d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x08\xe8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x10\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x80\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x38\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe0\x01\x00\x02\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x03\x00\x04\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x08\xf8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x40\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x10\xf0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x80\x80\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProblem","%start_pBlock","%start_pListBlock","%start_pTInt","%start_pTDouble","%start_pNumber","%start_pVal","%start_pDim","%start_pShape","%start_pVariableDecl","%start_pListVariableDecl","%start_pListListVariableDecl","%start_pConstantDecl","%start_pListConstantDecl","%start_pListListConstantDecl","%start_pLetDecl","%start_pListLetDecl","%start_pListListLetDecl","%start_pBound","%start_pConstraintDecl","%start_pListConstraintDecl","%start_pListListConstraintDecl","%start_pRotateAmount","%start_pPiecewiseCase","%start_pListPiecewiseCase","%start_pExp","%start_pExp1","%start_pExp2","%start_pExp3","%start_pExp4","%start_pExp5","String","KWDataPattern","TokenSub","TokenPlus","TokenReIm","TokenMul","TokenDiv","TokenScale","TokenDot","TokenPower","TokenRotate","TokenCase","PInteger","PDouble","PIdent","Problem","Block","ListBlock","TInt","TDouble","Number","Val","Dim","Shape","VariableDecl","ListVariableDecl","ListListVariableDecl","ConstantDecl","ListConstantDecl","ListListConstantDecl","LetDecl","ListLetDecl","ListListLetDecl","Bound","ConstraintDecl","ListConstraintDecl","ListListConstraintDecl","RotateAmount","PiecewiseCase","ListPiecewiseCase","Exp","Exp1","Exp2","Exp3","Exp4","Exp5","'('","')'","','","'->'","':'","';'","'<='","'='","'=='","'>='","'Dataset'","'File'","'Pattern'","'Random'","'['","']'","'constant'","'constants'","'constraint'","'constraints'","'it'","'let'","'minimize'","'otherwise'","'variable'","'variables'","'{'","'}'","L_quoted","L_KWDataPattern","L_TokenSub","L_TokenPlus","L_TokenReIm","L_TokenMul","L_TokenDiv","L_TokenScale","L_TokenDot","L_TokenPower","L_TokenRotate","L_TokenCase","L_PInteger","L_PDouble","L_PIdent","%eof"]
        bit_start = st * 123
        bit_end = (st + 1) * 123
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..122]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x48\x02\x48\x02\x48\x02\x52\x01\xf6\xff\x20\x01\xda\x01\x31\x00\x31\x00\xe4\xff\xe4\xff\xe4\xff\xe4\xff\xe4\xff\xe4\xff\xe4\xff\xe4\xff\xe4\xff\x12\x02\x25\x00\x25\x00\x25\x00\x1a\x00\xc5\x00\xc5\x00\x25\x00\x56\x00\x56\x00\x56\x00\x56\x00\x5d\x00\x29\x00\x00\x00\x00\x00\x00\x00\x00\x00\x38\x00\x25\x00\x00\x00\x00\x00\x00\x00\x5d\x00\x50\x00\x5d\x00\x67\x00\x00\x00\x00\x00\x00\x00\x07\x00\x00\x00\xa7\x01\x6f\x00\x31\x02\x52\x00\x25\x00\xe7\xff\xa3\x00\x00\x00\xa0\x00\x7c\x00\xa7\x00\xb5\x00\xaf\x00\xaf\x00\x52\x01\xf3\x00\xe4\x00\xc4\x00\xcc\x00\xc4\x00\xc4\x00\xd3\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc4\x00\xf9\x00\x2e\x01\x1f\x01\x0a\x01\x0a\x01\x0a\x01\x35\x01\x5b\x01\x42\x01\x40\x01\x40\x01\x40\x01\x6e\x01\x7c\x01\xb3\x01\x7f\x01\x7f\x01\x7f\x01\x99\x01\x94\x01\x9d\x01\xb8\x01\x00\x00\xb8\x01\xfe\x01\x0d\x02\x18\x02\x00\x00\xfa\x01\xff\x01\x00\x02\x29\x02\x45\x02\x48\x02\x45\x02\x5b\x02\x6d\x02\x6e\x02\x6f\x02\x70\x02\x71\x02\x72\x02\x73\x02\x4e\x02\x4e\x02\x00\x00\x5e\x02\x60\x02\x61\x02\x62\x02\x63\x02\x65\x02\x66\x02\x67\x02\x00\x00\x00\x00\x00\x00\x69\x02\x6b\x02\x6b\x02\x74\x02\x76\x02\x54\x02\x54\x02\x7b\x02\x5f\x02\x5f\x02\x7e\x02\x6a\x02\x6a\x02\x25\x00\x56\x00\x56\x00\x56\x00\x12\x02\x12\x02\x12\x02\x00\x00\x00\x00\x25\x00\x25\x00\x3f\x01\x25\x00\x20\x01\xc5\x00\x56\x00\x56\x00\x00\x00\x00\x00\xfd\xff\x56\x00\x56\x00\x00\x00\x00\x00\x55\x00\x00\x00\x00\x00\x5d\x00\x00\x00\x83\x00\x00\x00\x00\x00\x00\x00\x52\x01\x6c\x02\x6c\x02\x75\x02\x4a\x01\x4a\x01\x00\x00\x85\x02\x01\x01\x00\x00\x52\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbf\x01\xbf\x01\xbf\x01\x01\x01\x00\x00\x00\x00\xda\x01\x00\x00\x00\x00\xda\x01\x00\x00\x00\x00\x00\x00\x00\x00\x88\x02\x8a\x02\x8b\x02\x00\x00\x77\x02\x77\x02\x25\x00\x25\x00\x77\x02\x25\x00\x77\x02\x77\x02\x78\x02\x7a\x02\xb8\x00\x7c\x02\x7d\x02\x7f\x02\x80\x02\x81\x02\x00\x00\x00\x00\x82\x02\x00\x00\x00\x00\xf5\x01\x25\x00\xc5\x00\x8c\x02\x00\x00\x84\x02\x01\x01\x00\x00\x52\x01\x8d\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8f\x02\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x8c\x01\x83\x02\xea\x01\x7a\x01\x12\x00\xde\x01\xf4\x01\x8e\x02\xe6\x01\x86\x01\x6c\x01\x27\x02\x74\x00\x00\x01\xa2\x01\x37\x00\xe0\x00\x97\x00\xa8\x01\x92\x00\x61\x00\xff\xff\x79\x02\x86\x02\xec\x01\x9f\x00\x22\x01\x5d\x01\x74\x01\xa5\x01\xf7\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa5\x00\x00\x00\x00\x00\x00\x00\x04\x00\x89\x02\x35\x00\x00\x00\x00\x00\x00\x00\x00\x00\x91\x02\x00\x00\x2f\x02\x91\x02\x19\x02\x3c\x02\xb6\x00\xcc\x01\x41\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x97\x01\x00\x00\x00\x00\x00\x00\xcc\x01\x00\x00\x00\x00\x4b\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4d\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4f\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x90\x02\x00\x00\x95\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x96\x02\x00\x00\x99\x02\x00\x00\x57\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa6\x02\x97\x02\xa8\x02\x00\x00\x93\x02\x30\x02\x3d\x02\x00\x00\xf0\x01\x45\x01\x00\x00\xff\x00\x15\x01\xda\x00\x2b\x01\x30\x01\x39\x01\xc8\x01\xcb\x01\xe1\x01\x00\x00\x00\x00\x0c\x00\x6e\x00\x00\x00\xe7\x00\x1b\x02\x43\x02\x66\x01\x6b\x01\x00\x00\x00\x00\xcc\x01\x81\x01\x98\x01\x00\x00\x00\x00\xb5\x01\x00\x00\x00\x00\x65\x00\x00\x00\xcc\x01\x00\x00\x00\x00\x00\x00\xb6\x01\xa1\x02\xa1\x02\x00\x00\x64\x02\x64\x02\x00\x00\x00\x00\xcc\x01\x00\x00\x08\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x68\x02\x68\x02\x68\x02\xcc\x01\x00\x00\x00\x00\x03\x02\x00\x00\x00\x00\x0f\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x17\x02\x1d\x02\x30\x00\x3d\x00\x32\x01\xed\x00\x34\x02\x37\x02\x00\x00\x00\x00\xcc\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xab\x02\x00\x00\x00\x00\x00\x00\xfe\x00\x49\x02\x00\x00\x00\x00\x00\x00\xcc\x01\x00\x00\x1e\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xba\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe0\xff\x82\xff\x83\xff\x81\xff\x00\x00\x00\x00\xd4\xff\xd3\xff\xd2\xff\x00\x00\x00\x00\x81\xff\x00\x00\x85\xff\xde\xff\xd6\xff\x00\x00\x89\xff\x00\x00\x8c\xff\x00\x00\x8f\xff\x00\x00\x00\x00\x93\xff\xd5\xff\x98\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa1\xff\x9f\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc6\xff\xc4\xff\xa6\xff\xc2\xff\xc1\xff\xa5\xff\x00\x00\x00\x00\xaa\xff\xa8\xff\x00\x00\x00\x00\x00\x00\xba\xff\xaf\xff\xad\xff\x00\x00\x00\x00\x00\x00\xba\xff\xb4\xff\xb2\xff\x00\x00\x00\x00\x00\x00\xb9\xff\x00\x00\x00\x00\x00\x00\xbc\xff\x00\x00\x00\x00\x00\x00\x00\x00\xbd\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc8\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd1\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc7\xff\xc5\xff\xc3\xff\x00\x00\x00\x00\x00\x00\x00\x00\xb8\xff\x00\x00\x00\x00\xb6\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdd\xff\xdc\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdb\xff\xda\xff\x00\x00\x00\x00\x00\x00\xd9\xff\xd8\xff\x00\x00\xd7\xff\x88\xff\x00\x00\x86\xff\x00\x00\x84\xff\x87\xff\x8b\xff\x00\x00\x8d\xff\x8e\xff\x00\x00\x90\xff\x91\xff\x97\xff\x00\x00\x99\xff\x9d\xff\x00\x00\xa0\xff\x9e\xff\xa4\xff\xa2\xff\xa3\xff\x95\xff\x96\xff\x94\xff\xab\xff\xa9\xff\xa7\xff\x00\x00\xae\xff\xac\xff\x00\x00\xb3\xff\xb1\xff\xb7\xff\xbb\xff\x00\x00\x00\x00\x00\x00\xdf\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbe\xff\xc0\xff\x00\x00\xb5\xff\xb0\xff\x00\x00\x00\x00\x00\x00\x00\x00\x8a\xff\x00\x00\x9a\xff\x9c\xff\x00\x00\x00\x00\xcd\xff\xce\xff\xcb\xff\xcc\xff\xca\xff\xc9\xff\xcf\xff\xd0\xff\xbf\xff\x00\x00\x92\xff\x9b\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x02\x00\x05\x00\x0c\x00\x0d\x00\x0e\x00\x1f\x00\x20\x00\x21\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x02\x00\x2b\x00\x0c\x00\x0d\x00\x0e\x00\x2c\x00\x02\x00\x1f\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x01\x00\x1f\x00\x20\x00\x21\x00\x0d\x00\x2a\x00\x22\x00\x23\x00\x24\x00\x2d\x00\x13\x00\x01\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x26\x00\x22\x00\x23\x00\x24\x00\x2d\x00\x02\x00\x2c\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x02\x00\x0f\x00\x0c\x00\x0d\x00\x0e\x00\x1f\x00\x0e\x00\x1d\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x01\x00\x22\x00\x23\x00\x24\x00\x1e\x00\x01\x00\x01\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x01\x00\x22\x00\x23\x00\x24\x00\x2d\x00\x02\x00\x2c\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x02\x00\x0c\x00\x0d\x00\x0e\x00\x1f\x00\x1f\x00\x24\x00\x25\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x27\x00\x29\x00\x29\x00\x2a\x00\x2b\x00\x0e\x00\x22\x00\x23\x00\x02\x00\x29\x00\x2a\x00\x2b\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x1b\x00\x22\x00\x23\x00\x2d\x00\x2c\x00\x02\x00\x26\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x02\x00\x1f\x00\x20\x00\x21\x00\x0e\x00\x06\x00\x02\x00\x2c\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x07\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x22\x00\x1e\x00\x1f\x00\x20\x00\x02\x00\x04\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x22\x00\x23\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x07\x00\x1c\x00\x09\x00\x0a\x00\x1f\x00\x20\x00\x21\x00\x15\x00\x2c\x00\x02\x00\x18\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x02\x00\x06\x00\x1f\x00\x20\x00\x21\x00\x0e\x00\x02\x00\x2c\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x03\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x29\x00\x2a\x00\x1e\x00\x1f\x00\x02\x00\x08\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0e\x00\x0e\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x1b\x00\x1c\x00\x1e\x00\x1f\x00\x20\x00\x1f\x00\x20\x00\x21\x00\x0e\x00\x02\x00\x06\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x0a\x00\x02\x00\x0c\x00\x0d\x00\x0e\x00\x03\x00\x02\x00\x1e\x00\x1f\x00\x0a\x00\x2c\x00\x0c\x00\x0d\x00\x0e\x00\x0a\x00\x02\x00\x0c\x00\x0d\x00\x0e\x00\x1f\x00\x0e\x00\x02\x00\x03\x00\x0a\x00\x0f\x00\x0c\x00\x0d\x00\x0e\x00\x06\x00\x29\x00\x2a\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x1e\x00\x1f\x00\x20\x00\x0e\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x03\x00\x02\x00\x1b\x00\x1c\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x0a\x00\x02\x00\x0c\x00\x0d\x00\x0e\x00\x2c\x00\x02\x00\x24\x00\x25\x00\x0a\x00\x1f\x00\x0c\x00\x0d\x00\x0e\x00\x0a\x00\x02\x00\x0c\x00\x0d\x00\x0e\x00\x0e\x00\x29\x00\x02\x00\x0f\x00\x0a\x00\x03\x00\x0c\x00\x0d\x00\x0e\x00\x02\x00\x18\x00\x19\x00\x0c\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x0a\x00\x12\x00\x0c\x00\x0d\x00\x0e\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x0e\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x02\x00\x02\x00\x0f\x00\x10\x00\x11\x00\x18\x00\x2b\x00\x2c\x00\x2d\x00\x0a\x00\x0c\x00\x0c\x00\x0d\x00\x0e\x00\x02\x00\x0f\x00\x12\x00\x02\x00\x2c\x00\x2b\x00\x2c\x00\x2d\x00\x0a\x00\x0e\x00\x0c\x00\x0d\x00\x0e\x00\x0c\x00\x0d\x00\x0e\x00\x02\x00\x02\x00\x06\x00\x12\x00\x13\x00\x14\x00\x1b\x00\x1c\x00\x1d\x00\x2c\x00\x0c\x00\x0c\x00\x2b\x00\x2c\x00\x2d\x00\x29\x00\x12\x00\x12\x00\x21\x00\x02\x00\x24\x00\x25\x00\x02\x00\x02\x00\x03\x00\x04\x00\x2c\x00\x2d\x00\x2c\x00\x0c\x00\x0d\x00\x0e\x00\x0c\x00\x0d\x00\x0e\x00\x12\x00\x13\x00\x14\x00\x12\x00\x13\x00\x14\x00\x02\x00\x22\x00\x23\x00\x02\x00\x2c\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x21\x00\x0c\x00\x0d\x00\x21\x00\x0c\x00\x0d\x00\x0e\x00\x12\x00\x13\x00\x14\x00\x12\x00\x13\x00\x14\x00\x02\x00\x02\x00\x03\x00\x1f\x00\x10\x00\x11\x00\x16\x00\x17\x00\x0e\x00\x01\x00\x0c\x00\x0d\x00\x21\x00\x29\x00\x2a\x00\x02\x00\x12\x00\x13\x00\x14\x00\x15\x00\x02\x00\x1b\x00\x1c\x00\x1d\x00\x01\x00\x0c\x00\x0d\x00\x02\x00\x26\x00\x27\x00\x0c\x00\x12\x00\x13\x00\x14\x00\x15\x00\x01\x00\x12\x00\x0c\x00\x0d\x00\x02\x00\x05\x00\x06\x00\x02\x00\x12\x00\x13\x00\x14\x00\x15\x00\x0e\x00\x2c\x00\x0c\x00\x0d\x00\x2a\x00\x0c\x00\x0e\x00\x2c\x00\x12\x00\x13\x00\x14\x00\x12\x00\x1f\x00\x1b\x00\x1c\x00\x1d\x00\x0e\x00\x07\x00\x08\x00\x1b\x00\x1c\x00\x1d\x00\x29\x00\x2a\x00\x2b\x00\x0e\x00\x18\x00\x19\x00\x1a\x00\x0e\x00\x07\x00\x08\x00\x0e\x00\x05\x00\x06\x00\x18\x00\x19\x00\x1a\x00\x0e\x00\x18\x00\x19\x00\x1a\x00\x18\x00\x19\x00\x1a\x00\x29\x00\x22\x00\x23\x00\x18\x00\x19\x00\x0c\x00\x0d\x00\x11\x00\x12\x00\x13\x00\x14\x00\x2c\x00\x16\x00\x17\x00\x05\x00\x19\x00\x1a\x00\x16\x00\x17\x00\x16\x00\x17\x00\x10\x00\x11\x00\x26\x00\x27\x00\x07\x00\x08\x00\x05\x00\x06\x00\x26\x00\x27\x00\x2c\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x1b\x00\x2c\x00\x1b\x00\x1b\x00\x1b\x00\x1b\x00\x2b\x00\x1b\x00\x1b\x00\x1b\x00\x08\x00\x10\x00\x0f\x00\x08\x00\x1e\x00\x1d\x00\x04\x00\x2b\x00\x03\x00\x02\x00\x02\x00\x02\x00\x02\x00\x1b\x00\x02\x00\x26\x00\x10\x00\x1c\x00\x2b\x00\x1c\x00\x00\x00\x1c\x00\x1c\x00\x09\x00\x1c\x00\x1c\x00\x1c\x00\x25\x00\x1d\x00\x1c\x00\x0c\x00\x2b\x00\x0d\x00\x16\x00\x0c\x00\x16\x00\x01\x00\x00\x00\x16\x00\x09\x00\x00\x00\x26\x00\xff\xff\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x29\x00\xba\x00\x21\x00\x22\x00\x23\x00\x2f\x00\x9d\x00\x9e\x00\x2a\x00\x36\x00\x21\x00\x22\x00\x2b\x00\x29\x00\x29\x00\x21\x00\x22\x00\x23\x00\xff\xff\x6c\x00\x2f\x00\x2a\x00\x36\x00\x21\x00\x22\x00\x2b\x00\x41\x00\x2f\x00\x9d\x00\x9e\x00\x49\x00\x28\x00\x41\x00\x42\x00\x43\x00\x24\x00\x6d\x00\x26\x00\x44\x00\x38\x00\x35\x00\x33\x00\x31\x00\x2d\x00\xaf\x00\x41\x00\x42\x00\xc2\x00\xb1\x00\x29\x00\xff\xff\x44\x00\x38\x00\x35\x00\x33\x00\x31\x00\x2d\x00\x2a\x00\x36\x00\x21\x00\x22\x00\x2b\x00\x29\x00\x64\x00\x21\x00\x22\x00\x23\x00\x2f\x00\x4f\x00\x21\x00\x2a\x00\x36\x00\x21\x00\x22\x00\x2b\x00\x30\x00\x3a\x00\x27\x00\x28\x00\x29\x00\x41\x00\x41\x00\x42\x00\xe5\x00\x54\x00\xb7\x00\x26\x00\x44\x00\x38\x00\x35\x00\x33\x00\x31\x00\x2d\x00\x26\x00\x41\x00\x42\x00\xe4\x00\xaf\x00\x29\x00\xff\xff\x44\x00\x38\x00\x35\x00\x33\x00\x31\x00\x2d\x00\x2a\x00\x36\x00\x21\x00\x22\x00\x2b\x00\x29\x00\x21\x00\x22\x00\x23\x00\x2f\x00\x2f\x00\xac\x00\xad\x00\x2a\x00\x36\x00\x21\x00\x22\x00\x2b\x00\x30\x00\x27\x00\x27\x00\x28\x00\x29\x00\x55\x00\x41\x00\x45\x00\xb4\x00\x27\x00\x28\x00\x29\x00\x44\x00\x38\x00\x35\x00\x33\x00\x31\x00\x2d\x00\x5a\x00\x41\x00\xc1\x00\xb4\x00\xff\xff\x29\x00\xaf\x00\x44\x00\x38\x00\x35\x00\x33\x00\x31\x00\x2d\x00\x2a\x00\x36\x00\x21\x00\x22\x00\x2b\x00\x29\x00\x2f\x00\x9d\x00\x9e\x00\x4f\x00\xa4\x00\x29\x00\xff\xff\x2a\x00\x36\x00\x21\x00\x22\x00\x2b\x00\xa3\x00\x2a\x00\x36\x00\x21\x00\x22\x00\x2b\x00\x46\x00\x50\x00\x51\x00\x52\x00\x29\x00\xa2\x00\x44\x00\x38\x00\x35\x00\x33\x00\x31\x00\x2d\x00\x2a\x00\x36\x00\x21\x00\x22\x00\x2b\x00\xa7\x00\xa8\x00\x37\x00\x38\x00\x35\x00\x33\x00\x31\x00\x2d\x00\xb2\x00\x38\x00\x35\x00\x33\x00\x31\x00\x2d\x00\x9a\x00\xfd\x00\x9b\x00\x9c\x00\x2f\x00\x9d\x00\x9e\x00\x3d\x00\xff\xff\x29\x00\x3e\x00\xa8\x00\x38\x00\x35\x00\x33\x00\x31\x00\x2d\x00\x2a\x00\x36\x00\x21\x00\x22\x00\x2b\x00\x29\x00\x9f\x00\x2f\x00\x9d\x00\x9e\x00\x4f\x00\x29\x00\xff\xff\x2a\x00\x36\x00\x21\x00\x22\x00\x2b\x00\xa0\x00\x2a\x00\x36\x00\x21\x00\x22\x00\x2b\x00\x27\x00\x28\x00\x50\x00\x53\x00\x29\x00\x96\x00\xc9\x00\x38\x00\x35\x00\x33\x00\x31\x00\x2d\x00\x2a\x00\x36\x00\x21\x00\x22\x00\x2b\x00\x4f\x00\x55\x00\xbe\x00\x38\x00\x35\x00\x33\x00\x31\x00\x2d\x00\xe2\x00\x38\x00\x35\x00\x33\x00\x31\x00\x2d\x00\x56\x00\x59\x00\x50\x00\x51\x00\xcb\x00\x2f\x00\x9d\x00\x9e\x00\x4f\x00\x29\x00\x94\x00\xf3\x00\x38\x00\x35\x00\x33\x00\x31\x00\x2d\x00\x2a\x00\x29\x00\x21\x00\x22\x00\x2b\x00\x95\x00\x29\x00\x50\x00\xca\x00\x2a\x00\xff\xff\x21\x00\x22\x00\x2b\x00\x2a\x00\x29\x00\x21\x00\x22\x00\x2b\x00\x2f\x00\x4f\x00\xc0\x00\xc1\x00\x2a\x00\x64\x00\x21\x00\x22\x00\x2b\x00\x91\x00\x27\x00\x28\x00\x34\x00\x35\x00\x33\x00\x31\x00\x2d\x00\x50\x00\x51\x00\xe3\x00\x55\x00\xc8\x00\x35\x00\x33\x00\x31\x00\x2d\x00\xc7\x00\x35\x00\x33\x00\x31\x00\x2d\x00\x92\x00\x29\x00\x56\x00\xcd\x00\xc6\x00\x35\x00\x33\x00\x31\x00\x2d\x00\x2a\x00\x29\x00\x21\x00\x22\x00\x2b\x00\xff\xff\x29\x00\xac\x00\xad\x00\x2a\x00\x2f\x00\x21\x00\x22\x00\x2b\x00\x2a\x00\x29\x00\x21\x00\x22\x00\x2b\x00\x5b\x00\x27\x00\x6e\x00\x64\x00\x2a\x00\x8f\x00\x21\x00\x22\x00\x2b\x00\x29\x00\x5c\x00\x5f\x00\x48\x00\x32\x00\x33\x00\x31\x00\x2d\x00\x2a\x00\x6f\x00\x21\x00\x22\x00\x2b\x00\xbb\x00\x33\x00\x31\x00\x2d\x00\x5b\x00\xba\x00\x33\x00\x31\x00\x2d\x00\x6e\x00\x29\x00\x7b\x00\x70\x00\x7c\x00\x60\x00\x30\x00\x31\x00\x2d\x00\x2a\x00\x48\x00\x21\x00\x22\x00\x2b\x00\x29\x00\x64\x00\xa0\x00\x47\x00\xff\xff\xb8\x00\x31\x00\x2d\x00\x2a\x00\x55\x00\x21\x00\x22\x00\x2b\x00\x48\x00\x49\x00\x4a\x00\x6e\x00\x6e\x00\x8e\x00\x4b\x00\x4c\x00\x4d\x00\x56\x00\x57\x00\x58\x00\xff\xff\x48\x00\x48\x00\xb7\x00\x31\x00\x2d\x00\x27\x00\xb5\x00\xf0\x00\x4e\x00\x47\x00\xac\x00\xad\x00\x47\x00\x96\x00\x97\x00\x98\x00\x2c\x00\x2d\x00\xff\xff\x48\x00\x49\x00\x4a\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4b\x00\x4c\x00\x4d\x00\x47\x00\xa7\x00\xa8\x00\x47\x00\xff\xff\x68\x00\x69\x00\x6a\x00\x6b\x00\xc5\x00\x48\x00\x49\x00\xc4\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x6b\x00\x4b\x00\x4c\x00\x4d\x00\x47\x00\xf5\x00\xf6\x00\x2f\x00\x70\x00\x71\x00\x61\x00\x62\x00\x55\x00\x8b\x00\x48\x00\x49\x00\xc3\x00\x27\x00\x28\x00\x47\x00\x4b\x00\x4c\x00\x65\x00\x66\x00\x6e\x00\x56\x00\x57\x00\xce\x00\x8a\x00\x48\x00\x49\x00\x47\x00\x3a\x00\x3b\x00\x48\x00\x4b\x00\x4c\x00\x65\x00\xec\x00\x89\x00\xed\x00\x48\x00\x49\x00\x47\x00\xa4\x00\xa5\x00\x6e\x00\x4b\x00\x4c\x00\x65\x00\xeb\x00\x55\x00\xff\xff\x48\x00\x49\x00\x28\x00\x48\x00\x55\x00\xff\xff\x4b\x00\x4c\x00\xbd\x00\x00\x01\x2f\x00\x56\x00\x57\x00\xe7\x00\x5b\x00\xa9\x00\xaa\x00\x56\x00\x57\x00\xe6\x00\x27\x00\x28\x00\x29\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x5b\x00\xa9\x00\xaa\x00\x5b\x00\xa4\x00\xa5\x00\x5c\x00\x5d\x00\xd1\x00\x5b\x00\x5c\x00\x5d\x00\xe1\x00\x5c\x00\x5d\x00\xe0\x00\x27\x00\xa7\x00\xa8\x00\x5c\x00\xd0\x00\x86\x00\x87\x00\x73\x00\x74\x00\x75\x00\x76\x00\xff\xff\x77\x00\x78\x00\x85\x00\x79\x00\x7a\x00\x61\x00\x92\x00\x61\x00\x8f\x00\x70\x00\x85\x00\x3a\x00\xbc\x00\xa9\x00\xaa\x00\xa4\x00\xa5\x00\x3a\x00\xf2\x00\xff\xff\x84\x00\x83\x00\x82\x00\x81\x00\x80\x00\x7f\x00\x7e\x00\xe0\x00\xff\xff\xdf\x00\xde\x00\xdd\x00\xdc\x00\x29\x00\xdb\x00\xda\x00\xd9\x00\xd0\x00\xd4\x00\x64\x00\xcd\x00\xd8\x00\x21\x00\xef\x00\x29\x00\xeb\x00\xea\x00\xe9\x00\xf2\x00\x00\x01\xf0\x00\x03\x01\xaf\x00\x7a\x00\xff\x00\x29\x00\xfe\x00\xd5\x00\xfc\x00\xfb\x00\xad\x00\xfa\x00\xf9\x00\xf8\x00\x3f\x00\x21\x00\x02\x01\x8b\x00\x29\x00\x87\x00\x64\x00\x86\x00\x8c\x00\xd6\x00\xd4\x00\xd2\x00\xad\x00\xf6\x00\x3e\x00\x00\x00\xb0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (31, 126) [
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98),
	(99 , happyReduce_99),
	(100 , happyReduce_100),
	(101 , happyReduce_101),
	(102 , happyReduce_102),
	(103 , happyReduce_103),
	(104 , happyReduce_104),
	(105 , happyReduce_105),
	(106 , happyReduce_106),
	(107 , happyReduce_107),
	(108 , happyReduce_108),
	(109 , happyReduce_109),
	(110 , happyReduce_110),
	(111 , happyReduce_111),
	(112 , happyReduce_112),
	(113 , happyReduce_113),
	(114 , happyReduce_114),
	(115 , happyReduce_115),
	(116 , happyReduce_116),
	(117 , happyReduce_117),
	(118 , happyReduce_118),
	(119 , happyReduce_119),
	(120 , happyReduce_120),
	(121 , happyReduce_121),
	(122 , happyReduce_122),
	(123 , happyReduce_123),
	(124 , happyReduce_124),
	(125 , happyReduce_125),
	(126 , happyReduce_126)
	]

happy_n_terms = 45 :: Int
happy_n_nonterms = 46 :: Int

happyReduce_31 = happySpecReduce_1  0# happyReduction_31
happyReduction_31 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TL happy_var_1)) -> 
	happyIn34
		 (happy_var_1
	)}

happyReduce_32 = happySpecReduce_1  1# happyReduction_32
happyReduction_32 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (T_KWDataPattern happy_var_1)) -> 
	happyIn35
		 (KWDataPattern (happy_var_1)
	)}

happyReduce_33 = happySpecReduce_1  2# happyReduction_33
happyReduction_33 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn36
		 (TokenSub (mkPosToken happy_var_1)
	)}

happyReduce_34 = happySpecReduce_1  3# happyReduction_34
happyReduction_34 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn37
		 (TokenPlus (mkPosToken happy_var_1)
	)}

happyReduce_35 = happySpecReduce_1  4# happyReduction_35
happyReduction_35 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn38
		 (TokenReIm (mkPosToken happy_var_1)
	)}

happyReduce_36 = happySpecReduce_1  5# happyReduction_36
happyReduction_36 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn39
		 (TokenMul (mkPosToken happy_var_1)
	)}

happyReduce_37 = happySpecReduce_1  6# happyReduction_37
happyReduction_37 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn40
		 (TokenDiv (mkPosToken happy_var_1)
	)}

happyReduce_38 = happySpecReduce_1  7# happyReduction_38
happyReduction_38 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn41
		 (TokenScale (mkPosToken happy_var_1)
	)}

happyReduce_39 = happySpecReduce_1  8# happyReduction_39
happyReduction_39 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn42
		 (TokenDot (mkPosToken happy_var_1)
	)}

happyReduce_40 = happySpecReduce_1  9# happyReduction_40
happyReduction_40 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn43
		 (TokenPower (mkPosToken happy_var_1)
	)}

happyReduce_41 = happySpecReduce_1  10# happyReduction_41
happyReduction_41 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn44
		 (TokenRotate (mkPosToken happy_var_1)
	)}

happyReduce_42 = happySpecReduce_1  11# happyReduction_42
happyReduction_42 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn45
		 (TokenCase (mkPosToken happy_var_1)
	)}

happyReduce_43 = happySpecReduce_1  12# happyReduction_43
happyReduction_43 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn46
		 (PInteger (mkPosToken happy_var_1)
	)}

happyReduce_44 = happySpecReduce_1  13# happyReduction_44
happyReduction_44 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn47
		 (PDouble (mkPosToken happy_var_1)
	)}

happyReduce_45 = happySpecReduce_1  14# happyReduction_45
happyReduction_45 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn48
		 (PIdent (mkPosToken happy_var_1)
	)}

happyReduce_46 = happySpecReduce_1  15# happyReduction_46
happyReduction_46 happy_x_1
	 =  case happyOut51 happy_x_1 of { (HappyWrap51 happy_var_1) -> 
	happyIn49
		 (AbsHashedLang.Problem happy_var_1
	)}

happyReduce_47 = happyReduce 5# 16# happyReduction_47
happyReduction_47 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut60 happy_x_4 of { (HappyWrap60 happy_var_4) -> 
	happyIn50
		 (AbsHashedLang.BlockVariable happy_var_4
	) `HappyStk` happyRest}

happyReduce_48 = happyReduce 5# 16# happyReduction_48
happyReduction_48 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut60 happy_x_4 of { (HappyWrap60 happy_var_4) -> 
	happyIn50
		 (AbsHashedLang.BlockVariable happy_var_4
	) `HappyStk` happyRest}

happyReduce_49 = happyReduce 5# 16# happyReduction_49
happyReduction_49 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut63 happy_x_4 of { (HappyWrap63 happy_var_4) -> 
	happyIn50
		 (AbsHashedLang.BlockConstant happy_var_4
	) `HappyStk` happyRest}

happyReduce_50 = happyReduce 5# 16# happyReduction_50
happyReduction_50 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut63 happy_x_4 of { (HappyWrap63 happy_var_4) -> 
	happyIn50
		 (AbsHashedLang.BlockConstant happy_var_4
	) `HappyStk` happyRest}

happyReduce_51 = happyReduce 5# 16# happyReduction_51
happyReduction_51 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut70 happy_x_4 of { (HappyWrap70 happy_var_4) -> 
	happyIn50
		 (AbsHashedLang.BlockConstraint happy_var_4
	) `HappyStk` happyRest}

happyReduce_52 = happyReduce 5# 16# happyReduction_52
happyReduction_52 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut70 happy_x_4 of { (HappyWrap70 happy_var_4) -> 
	happyIn50
		 (AbsHashedLang.BlockConstraint happy_var_4
	) `HappyStk` happyRest}

happyReduce_53 = happyReduce 5# 16# happyReduction_53
happyReduction_53 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut66 happy_x_4 of { (HappyWrap66 happy_var_4) -> 
	happyIn50
		 (AbsHashedLang.BlockLet happy_var_4
	) `HappyStk` happyRest}

happyReduce_54 = happyReduce 5# 16# happyReduction_54
happyReduction_54 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut74 happy_x_4 of { (HappyWrap74 happy_var_4) -> 
	happyIn50
		 (AbsHashedLang.BlockMinimize happy_var_4
	) `HappyStk` happyRest}

happyReduce_55 = happySpecReduce_1  17# happyReduction_55
happyReduction_55 happy_x_1
	 =  case happyOut50 happy_x_1 of { (HappyWrap50 happy_var_1) -> 
	happyIn51
		 ((:[]) happy_var_1
	)}

happyReduce_56 = happySpecReduce_2  17# happyReduction_56
happyReduction_56 happy_x_2
	happy_x_1
	 =  case happyOut50 happy_x_1 of { (HappyWrap50 happy_var_1) -> 
	case happyOut51 happy_x_2 of { (HappyWrap51 happy_var_2) -> 
	happyIn51
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_57 = happySpecReduce_1  18# happyReduction_57
happyReduction_57 happy_x_1
	 =  case happyOut46 happy_x_1 of { (HappyWrap46 happy_var_1) -> 
	happyIn52
		 (AbsHashedLang.IntPos happy_var_1
	)}

happyReduce_58 = happySpecReduce_2  18# happyReduction_58
happyReduction_58 happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_1 of { (HappyWrap36 happy_var_1) -> 
	case happyOut46 happy_x_2 of { (HappyWrap46 happy_var_2) -> 
	happyIn52
		 (AbsHashedLang.IntNeg happy_var_1 happy_var_2
	)}}

happyReduce_59 = happySpecReduce_1  19# happyReduction_59
happyReduction_59 happy_x_1
	 =  case happyOut47 happy_x_1 of { (HappyWrap47 happy_var_1) -> 
	happyIn53
		 (AbsHashedLang.DoublePos happy_var_1
	)}

happyReduce_60 = happySpecReduce_2  19# happyReduction_60
happyReduction_60 happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_1 of { (HappyWrap36 happy_var_1) -> 
	case happyOut47 happy_x_2 of { (HappyWrap47 happy_var_2) -> 
	happyIn53
		 (AbsHashedLang.DoubleNeg happy_var_1 happy_var_2
	)}}

happyReduce_61 = happySpecReduce_1  20# happyReduction_61
happyReduction_61 happy_x_1
	 =  case happyOut52 happy_x_1 of { (HappyWrap52 happy_var_1) -> 
	happyIn54
		 (AbsHashedLang.NumInt happy_var_1
	)}

happyReduce_62 = happySpecReduce_1  20# happyReduction_62
happyReduction_62 happy_x_1
	 =  case happyOut53 happy_x_1 of { (HappyWrap53 happy_var_1) -> 
	happyIn54
		 (AbsHashedLang.NumDouble happy_var_1
	)}

happyReduce_63 = happyReduce 4# 21# happyReduction_63
happyReduction_63 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut34 happy_x_3 of { (HappyWrap34 happy_var_3) -> 
	happyIn55
		 (AbsHashedLang.ValFile happy_var_3
	) `HappyStk` happyRest}

happyReduce_64 = happyReduce 6# 21# happyReduction_64
happyReduction_64 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut34 happy_x_3 of { (HappyWrap34 happy_var_3) -> 
	case happyOut34 happy_x_5 of { (HappyWrap34 happy_var_5) -> 
	happyIn55
		 (AbsHashedLang.ValDataset happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_65 = happyReduce 4# 21# happyReduction_65
happyReduction_65 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut35 happy_x_3 of { (HappyWrap35 happy_var_3) -> 
	happyIn55
		 (AbsHashedLang.ValPattern happy_var_3
	) `HappyStk` happyRest}

happyReduce_66 = happySpecReduce_1  21# happyReduction_66
happyReduction_66 happy_x_1
	 =  happyIn55
		 (AbsHashedLang.ValRandom
	)

happyReduce_67 = happySpecReduce_1  21# happyReduction_67
happyReduction_67 happy_x_1
	 =  case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	happyIn55
		 (AbsHashedLang.ValLiteral happy_var_1
	)}

happyReduce_68 = happySpecReduce_3  22# happyReduction_68
happyReduction_68 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_2 of { (HappyWrap46 happy_var_2) -> 
	happyIn56
		 (AbsHashedLang.Dim happy_var_2
	)}

happyReduce_69 = happySpecReduce_0  23# happyReduction_69
happyReduction_69  =  happyIn57
		 (AbsHashedLang.ShapeScalar
	)

happyReduce_70 = happySpecReduce_1  23# happyReduction_70
happyReduction_70 happy_x_1
	 =  case happyOut56 happy_x_1 of { (HappyWrap56 happy_var_1) -> 
	happyIn57
		 (AbsHashedLang.Shape1D happy_var_1
	)}

happyReduce_71 = happySpecReduce_2  23# happyReduction_71
happyReduction_71 happy_x_2
	happy_x_1
	 =  case happyOut56 happy_x_1 of { (HappyWrap56 happy_var_1) -> 
	case happyOut56 happy_x_2 of { (HappyWrap56 happy_var_2) -> 
	happyIn57
		 (AbsHashedLang.Shape2D happy_var_1 happy_var_2
	)}}

happyReduce_72 = happySpecReduce_3  23# happyReduction_72
happyReduction_72 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut56 happy_x_1 of { (HappyWrap56 happy_var_1) -> 
	case happyOut56 happy_x_2 of { (HappyWrap56 happy_var_2) -> 
	case happyOut56 happy_x_3 of { (HappyWrap56 happy_var_3) -> 
	happyIn57
		 (AbsHashedLang.Shape3D happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_73 = happySpecReduce_2  24# happyReduction_73
happyReduction_73 happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_1 of { (HappyWrap48 happy_var_1) -> 
	case happyOut57 happy_x_2 of { (HappyWrap57 happy_var_2) -> 
	happyIn58
		 (AbsHashedLang.VariableNoInit happy_var_1 happy_var_2
	)}}

happyReduce_74 = happyReduce 4# 24# happyReduction_74
happyReduction_74 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut48 happy_x_1 of { (HappyWrap48 happy_var_1) -> 
	case happyOut57 happy_x_2 of { (HappyWrap57 happy_var_2) -> 
	case happyOut55 happy_x_4 of { (HappyWrap55 happy_var_4) -> 
	happyIn58
		 (AbsHashedLang.VariableWithInit happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_75 = happySpecReduce_1  25# happyReduction_75
happyReduction_75 happy_x_1
	 =  case happyOut58 happy_x_1 of { (HappyWrap58 happy_var_1) -> 
	happyIn59
		 ((:[]) happy_var_1
	)}

happyReduce_76 = happySpecReduce_3  25# happyReduction_76
happyReduction_76 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut58 happy_x_1 of { (HappyWrap58 happy_var_1) -> 
	case happyOut59 happy_x_3 of { (HappyWrap59 happy_var_3) -> 
	happyIn59
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_77 = happySpecReduce_1  26# happyReduction_77
happyReduction_77 happy_x_1
	 =  case happyOut59 happy_x_1 of { (HappyWrap59 happy_var_1) -> 
	happyIn60
		 ((:[]) happy_var_1
	)}

happyReduce_78 = happySpecReduce_3  26# happyReduction_78
happyReduction_78 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut59 happy_x_1 of { (HappyWrap59 happy_var_1) -> 
	case happyOut60 happy_x_3 of { (HappyWrap60 happy_var_3) -> 
	happyIn60
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_79 = happyReduce 4# 27# happyReduction_79
happyReduction_79 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut48 happy_x_1 of { (HappyWrap48 happy_var_1) -> 
	case happyOut57 happy_x_2 of { (HappyWrap57 happy_var_2) -> 
	case happyOut55 happy_x_4 of { (HappyWrap55 happy_var_4) -> 
	happyIn61
		 (AbsHashedLang.ConstantDecl happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_80 = happySpecReduce_1  28# happyReduction_80
happyReduction_80 happy_x_1
	 =  case happyOut61 happy_x_1 of { (HappyWrap61 happy_var_1) -> 
	happyIn62
		 ((:[]) happy_var_1
	)}

happyReduce_81 = happySpecReduce_3  28# happyReduction_81
happyReduction_81 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut61 happy_x_1 of { (HappyWrap61 happy_var_1) -> 
	case happyOut62 happy_x_3 of { (HappyWrap62 happy_var_3) -> 
	happyIn62
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_82 = happySpecReduce_1  29# happyReduction_82
happyReduction_82 happy_x_1
	 =  case happyOut62 happy_x_1 of { (HappyWrap62 happy_var_1) -> 
	happyIn63
		 ((:[]) happy_var_1
	)}

happyReduce_83 = happySpecReduce_3  29# happyReduction_83
happyReduction_83 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut62 happy_x_1 of { (HappyWrap62 happy_var_1) -> 
	case happyOut63 happy_x_3 of { (HappyWrap63 happy_var_3) -> 
	happyIn63
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_84 = happySpecReduce_3  30# happyReduction_84
happyReduction_84 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_1 of { (HappyWrap48 happy_var_1) -> 
	case happyOut74 happy_x_3 of { (HappyWrap74 happy_var_3) -> 
	happyIn64
		 (AbsHashedLang.LetDecl happy_var_1 happy_var_3
	)}}

happyReduce_85 = happySpecReduce_1  31# happyReduction_85
happyReduction_85 happy_x_1
	 =  case happyOut64 happy_x_1 of { (HappyWrap64 happy_var_1) -> 
	happyIn65
		 ((:[]) happy_var_1
	)}

happyReduce_86 = happySpecReduce_3  31# happyReduction_86
happyReduction_86 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut64 happy_x_1 of { (HappyWrap64 happy_var_1) -> 
	case happyOut65 happy_x_3 of { (HappyWrap65 happy_var_3) -> 
	happyIn65
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_87 = happySpecReduce_1  32# happyReduction_87
happyReduction_87 happy_x_1
	 =  case happyOut65 happy_x_1 of { (HappyWrap65 happy_var_1) -> 
	happyIn66
		 ((:[]) happy_var_1
	)}

happyReduce_88 = happySpecReduce_3  32# happyReduction_88
happyReduction_88 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut65 happy_x_1 of { (HappyWrap65 happy_var_1) -> 
	case happyOut66 happy_x_3 of { (HappyWrap66 happy_var_3) -> 
	happyIn66
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_89 = happySpecReduce_1  33# happyReduction_89
happyReduction_89 happy_x_1
	 =  case happyOut48 happy_x_1 of { (HappyWrap48 happy_var_1) -> 
	happyIn67
		 (AbsHashedLang.ConstantBound happy_var_1
	)}

happyReduce_90 = happySpecReduce_1  33# happyReduction_90
happyReduction_90 happy_x_1
	 =  case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	happyIn67
		 (AbsHashedLang.NumberBound happy_var_1
	)}

happyReduce_91 = happySpecReduce_3  34# happyReduction_91
happyReduction_91 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut74 happy_x_1 of { (HappyWrap74 happy_var_1) -> 
	case happyOut67 happy_x_3 of { (HappyWrap67 happy_var_3) -> 
	happyIn68
		 (AbsHashedLang.ConstraintLower happy_var_1 happy_var_3
	)}}

happyReduce_92 = happySpecReduce_3  34# happyReduction_92
happyReduction_92 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut74 happy_x_1 of { (HappyWrap74 happy_var_1) -> 
	case happyOut67 happy_x_3 of { (HappyWrap67 happy_var_3) -> 
	happyIn68
		 (AbsHashedLang.ConstraintUpper happy_var_1 happy_var_3
	)}}

happyReduce_93 = happySpecReduce_3  34# happyReduction_93
happyReduction_93 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut74 happy_x_1 of { (HappyWrap74 happy_var_1) -> 
	case happyOut67 happy_x_3 of { (HappyWrap67 happy_var_3) -> 
	happyIn68
		 (AbsHashedLang.ConstraintEqual happy_var_1 happy_var_3
	)}}

happyReduce_94 = happySpecReduce_1  35# happyReduction_94
happyReduction_94 happy_x_1
	 =  case happyOut68 happy_x_1 of { (HappyWrap68 happy_var_1) -> 
	happyIn69
		 ((:[]) happy_var_1
	)}

happyReduce_95 = happySpecReduce_3  35# happyReduction_95
happyReduction_95 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut68 happy_x_1 of { (HappyWrap68 happy_var_1) -> 
	case happyOut69 happy_x_3 of { (HappyWrap69 happy_var_3) -> 
	happyIn69
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_96 = happySpecReduce_1  36# happyReduction_96
happyReduction_96 happy_x_1
	 =  case happyOut69 happy_x_1 of { (HappyWrap69 happy_var_1) -> 
	happyIn70
		 ((:[]) happy_var_1
	)}

happyReduce_97 = happySpecReduce_3  36# happyReduction_97
happyReduction_97 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut69 happy_x_1 of { (HappyWrap69 happy_var_1) -> 
	case happyOut70 happy_x_3 of { (HappyWrap70 happy_var_3) -> 
	happyIn70
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_98 = happySpecReduce_3  37# happyReduction_98
happyReduction_98 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut52 happy_x_2 of { (HappyWrap52 happy_var_2) -> 
	happyIn71
		 (AbsHashedLang.RA1D happy_var_2
	)}

happyReduce_99 = happyReduce 5# 37# happyReduction_99
happyReduction_99 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut52 happy_x_2 of { (HappyWrap52 happy_var_2) -> 
	case happyOut52 happy_x_4 of { (HappyWrap52 happy_var_4) -> 
	happyIn71
		 (AbsHashedLang.RA2D happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_100 = happyReduce 7# 37# happyReduction_100
happyReduction_100 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut52 happy_x_2 of { (HappyWrap52 happy_var_2) -> 
	case happyOut52 happy_x_4 of { (HappyWrap52 happy_var_4) -> 
	case happyOut52 happy_x_6 of { (HappyWrap52 happy_var_6) -> 
	happyIn71
		 (AbsHashedLang.RA3D happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_101 = happyReduce 5# 38# happyReduction_101
happyReduction_101 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut54 happy_x_3 of { (HappyWrap54 happy_var_3) -> 
	case happyOut74 happy_x_5 of { (HappyWrap74 happy_var_5) -> 
	happyIn72
		 (AbsHashedLang.PiecewiseCase happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_102 = happySpecReduce_3  38# happyReduction_102
happyReduction_102 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut74 happy_x_3 of { (HappyWrap74 happy_var_3) -> 
	happyIn72
		 (AbsHashedLang.PiecewiseFinalCase happy_var_3
	)}

happyReduce_103 = happySpecReduce_1  39# happyReduction_103
happyReduction_103 happy_x_1
	 =  case happyOut72 happy_x_1 of { (HappyWrap72 happy_var_1) -> 
	happyIn73
		 ((:[]) happy_var_1
	)}

happyReduce_104 = happySpecReduce_3  39# happyReduction_104
happyReduction_104 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut72 happy_x_1 of { (HappyWrap72 happy_var_1) -> 
	case happyOut73 happy_x_3 of { (HappyWrap73 happy_var_3) -> 
	happyIn73
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_105 = happySpecReduce_3  40# happyReduction_105
happyReduction_105 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut74 happy_x_1 of { (HappyWrap74 happy_var_1) -> 
	case happyOut37 happy_x_2 of { (HappyWrap37 happy_var_2) -> 
	case happyOut75 happy_x_3 of { (HappyWrap75 happy_var_3) -> 
	happyIn74
		 (AbsHashedLang.EPlus happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_106 = happySpecReduce_3  40# happyReduction_106
happyReduction_106 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut74 happy_x_1 of { (HappyWrap74 happy_var_1) -> 
	case happyOut38 happy_x_2 of { (HappyWrap38 happy_var_2) -> 
	case happyOut75 happy_x_3 of { (HappyWrap75 happy_var_3) -> 
	happyIn74
		 (AbsHashedLang.ERealImag happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_107 = happySpecReduce_3  40# happyReduction_107
happyReduction_107 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut74 happy_x_1 of { (HappyWrap74 happy_var_1) -> 
	case happyOut36 happy_x_2 of { (HappyWrap36 happy_var_2) -> 
	case happyOut75 happy_x_3 of { (HappyWrap75 happy_var_3) -> 
	happyIn74
		 (AbsHashedLang.ESubtract happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_108 = happySpecReduce_1  40# happyReduction_108
happyReduction_108 happy_x_1
	 =  case happyOut75 happy_x_1 of { (HappyWrap75 happy_var_1) -> 
	happyIn74
		 (happy_var_1
	)}

happyReduce_109 = happyReduce 6# 40# happyReduction_109
happyReduction_109 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut45 happy_x_1 of { (HappyWrap45 happy_var_1) -> 
	case happyOut74 happy_x_2 of { (HappyWrap74 happy_var_2) -> 
	case happyOut73 happy_x_5 of { (HappyWrap73 happy_var_5) -> 
	happyIn74
		 (AbsHashedLang.EPiecewise happy_var_1 happy_var_2 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_110 = happySpecReduce_3  41# happyReduction_110
happyReduction_110 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut75 happy_x_1 of { (HappyWrap75 happy_var_1) -> 
	case happyOut39 happy_x_2 of { (HappyWrap39 happy_var_2) -> 
	case happyOut76 happy_x_3 of { (HappyWrap76 happy_var_3) -> 
	happyIn75
		 (AbsHashedLang.EMul happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_111 = happySpecReduce_3  41# happyReduction_111
happyReduction_111 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut75 happy_x_1 of { (HappyWrap75 happy_var_1) -> 
	case happyOut40 happy_x_2 of { (HappyWrap40 happy_var_2) -> 
	case happyOut76 happy_x_3 of { (HappyWrap76 happy_var_3) -> 
	happyIn75
		 (AbsHashedLang.EDiv happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_112 = happySpecReduce_1  41# happyReduction_112
happyReduction_112 happy_x_1
	 =  case happyOut76 happy_x_1 of { (HappyWrap76 happy_var_1) -> 
	happyIn75
		 (happy_var_1
	)}

happyReduce_113 = happySpecReduce_3  42# happyReduction_113
happyReduction_113 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut76 happy_x_1 of { (HappyWrap76 happy_var_1) -> 
	case happyOut41 happy_x_2 of { (HappyWrap41 happy_var_2) -> 
	case happyOut77 happy_x_3 of { (HappyWrap77 happy_var_3) -> 
	happyIn76
		 (AbsHashedLang.EScale happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_114 = happySpecReduce_3  42# happyReduction_114
happyReduction_114 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut76 happy_x_1 of { (HappyWrap76 happy_var_1) -> 
	case happyOut42 happy_x_2 of { (HappyWrap42 happy_var_2) -> 
	case happyOut77 happy_x_3 of { (HappyWrap77 happy_var_3) -> 
	happyIn76
		 (AbsHashedLang.EDot happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_115 = happySpecReduce_1  42# happyReduction_115
happyReduction_115 happy_x_1
	 =  case happyOut77 happy_x_1 of { (HappyWrap77 happy_var_1) -> 
	happyIn76
		 (happy_var_1
	)}

happyReduce_116 = happySpecReduce_3  43# happyReduction_116
happyReduction_116 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut77 happy_x_1 of { (HappyWrap77 happy_var_1) -> 
	case happyOut43 happy_x_2 of { (HappyWrap43 happy_var_2) -> 
	case happyOut52 happy_x_3 of { (HappyWrap52 happy_var_3) -> 
	happyIn77
		 (AbsHashedLang.EPower happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_117 = happyReduce 5# 43# happyReduction_117
happyReduction_117 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut77 happy_x_1 of { (HappyWrap77 happy_var_1) -> 
	case happyOut43 happy_x_2 of { (HappyWrap43 happy_var_2) -> 
	case happyOut52 happy_x_4 of { (HappyWrap52 happy_var_4) -> 
	happyIn77
		 (AbsHashedLang.EPower happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_118 = happySpecReduce_1  43# happyReduction_118
happyReduction_118 happy_x_1
	 =  case happyOut78 happy_x_1 of { (HappyWrap78 happy_var_1) -> 
	happyIn77
		 (happy_var_1
	)}

happyReduce_119 = happySpecReduce_2  44# happyReduction_119
happyReduction_119 happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_1 of { (HappyWrap48 happy_var_1) -> 
	case happyOut79 happy_x_2 of { (HappyWrap79 happy_var_2) -> 
	happyIn78
		 (AbsHashedLang.EFun happy_var_1 happy_var_2
	)}}

happyReduce_120 = happySpecReduce_3  44# happyReduction_120
happyReduction_120 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut44 happy_x_1 of { (HappyWrap44 happy_var_1) -> 
	case happyOut71 happy_x_2 of { (HappyWrap71 happy_var_2) -> 
	case happyOut79 happy_x_3 of { (HappyWrap79 happy_var_3) -> 
	happyIn78
		 (AbsHashedLang.ERotate happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_121 = happySpecReduce_2  44# happyReduction_121
happyReduction_121 happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_1 of { (HappyWrap36 happy_var_1) -> 
	case happyOut79 happy_x_2 of { (HappyWrap79 happy_var_2) -> 
	happyIn78
		 (AbsHashedLang.ENegate happy_var_1 happy_var_2
	)}}

happyReduce_122 = happySpecReduce_1  44# happyReduction_122
happyReduction_122 happy_x_1
	 =  case happyOut79 happy_x_1 of { (HappyWrap79 happy_var_1) -> 
	happyIn78
		 (happy_var_1
	)}

happyReduce_123 = happySpecReduce_3  45# happyReduction_123
happyReduction_123 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut74 happy_x_2 of { (HappyWrap74 happy_var_2) -> 
	happyIn79
		 (happy_var_2
	)}

happyReduce_124 = happySpecReduce_1  45# happyReduction_124
happyReduction_124 happy_x_1
	 =  case happyOut47 happy_x_1 of { (HappyWrap47 happy_var_1) -> 
	happyIn79
		 (AbsHashedLang.ENumDouble happy_var_1
	)}

happyReduce_125 = happySpecReduce_1  45# happyReduction_125
happyReduction_125 happy_x_1
	 =  case happyOut46 happy_x_1 of { (HappyWrap46 happy_var_1) -> 
	happyIn79
		 (AbsHashedLang.ENumInteger happy_var_1
	)}

happyReduce_126 = happySpecReduce_1  45# happyReduction_126
happyReduction_126 happy_x_1
	 =  case happyOut48 happy_x_1 of { (HappyWrap48 happy_var_1) -> 
	happyIn79
		 (AbsHashedLang.EIdent happy_var_1
	)}

happyNewToken action sts stk [] =
	happyDoAction 44# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TS _ 16) -> cont 16#;
	PT _ (TS _ 17) -> cont 17#;
	PT _ (TS _ 18) -> cont 18#;
	PT _ (TS _ 19) -> cont 19#;
	PT _ (TS _ 20) -> cont 20#;
	PT _ (TS _ 21) -> cont 21#;
	PT _ (TS _ 22) -> cont 22#;
	PT _ (TS _ 23) -> cont 23#;
	PT _ (TS _ 24) -> cont 24#;
	PT _ (TS _ 25) -> cont 25#;
	PT _ (TS _ 26) -> cont 26#;
	PT _ (TS _ 27) -> cont 27#;
	PT _ (TS _ 28) -> cont 28#;
	PT _ (TL happy_dollar_dollar) -> cont 29#;
	PT _ (T_KWDataPattern happy_dollar_dollar) -> cont 30#;
	PT _ (T_TokenSub _) -> cont 31#;
	PT _ (T_TokenPlus _) -> cont 32#;
	PT _ (T_TokenReIm _) -> cont 33#;
	PT _ (T_TokenMul _) -> cont 34#;
	PT _ (T_TokenDiv _) -> cont 35#;
	PT _ (T_TokenScale _) -> cont 36#;
	PT _ (T_TokenDot _) -> cont 37#;
	PT _ (T_TokenPower _) -> cont 38#;
	PT _ (T_TokenRotate _) -> cont 39#;
	PT _ (T_TokenCase _) -> cont 40#;
	PT _ (T_PInteger _) -> cont 41#;
	PT _ (T_PDouble _) -> cont 42#;
	PT _ (T_PIdent _) -> cont 43#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 44# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => ([(Token)], [String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pProblem tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {(HappyWrap49 x') = happyOut49 x} in x'))

pBlock tks = happySomeParser where
 happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (let {(HappyWrap50 x') = happyOut50 x} in x'))

pListBlock tks = happySomeParser where
 happySomeParser = happyThen (happyParse 2# tks) (\x -> happyReturn (let {(HappyWrap51 x') = happyOut51 x} in x'))

pTInt tks = happySomeParser where
 happySomeParser = happyThen (happyParse 3# tks) (\x -> happyReturn (let {(HappyWrap52 x') = happyOut52 x} in x'))

pTDouble tks = happySomeParser where
 happySomeParser = happyThen (happyParse 4# tks) (\x -> happyReturn (let {(HappyWrap53 x') = happyOut53 x} in x'))

pNumber tks = happySomeParser where
 happySomeParser = happyThen (happyParse 5# tks) (\x -> happyReturn (let {(HappyWrap54 x') = happyOut54 x} in x'))

pVal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 6# tks) (\x -> happyReturn (let {(HappyWrap55 x') = happyOut55 x} in x'))

pDim tks = happySomeParser where
 happySomeParser = happyThen (happyParse 7# tks) (\x -> happyReturn (let {(HappyWrap56 x') = happyOut56 x} in x'))

pShape tks = happySomeParser where
 happySomeParser = happyThen (happyParse 8# tks) (\x -> happyReturn (let {(HappyWrap57 x') = happyOut57 x} in x'))

pVariableDecl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 9# tks) (\x -> happyReturn (let {(HappyWrap58 x') = happyOut58 x} in x'))

pListVariableDecl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 10# tks) (\x -> happyReturn (let {(HappyWrap59 x') = happyOut59 x} in x'))

pListListVariableDecl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 11# tks) (\x -> happyReturn (let {(HappyWrap60 x') = happyOut60 x} in x'))

pConstantDecl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 12# tks) (\x -> happyReturn (let {(HappyWrap61 x') = happyOut61 x} in x'))

pListConstantDecl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 13# tks) (\x -> happyReturn (let {(HappyWrap62 x') = happyOut62 x} in x'))

pListListConstantDecl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 14# tks) (\x -> happyReturn (let {(HappyWrap63 x') = happyOut63 x} in x'))

pLetDecl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 15# tks) (\x -> happyReturn (let {(HappyWrap64 x') = happyOut64 x} in x'))

pListLetDecl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 16# tks) (\x -> happyReturn (let {(HappyWrap65 x') = happyOut65 x} in x'))

pListListLetDecl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 17# tks) (\x -> happyReturn (let {(HappyWrap66 x') = happyOut66 x} in x'))

pBound tks = happySomeParser where
 happySomeParser = happyThen (happyParse 18# tks) (\x -> happyReturn (let {(HappyWrap67 x') = happyOut67 x} in x'))

pConstraintDecl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 19# tks) (\x -> happyReturn (let {(HappyWrap68 x') = happyOut68 x} in x'))

pListConstraintDecl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 20# tks) (\x -> happyReturn (let {(HappyWrap69 x') = happyOut69 x} in x'))

pListListConstraintDecl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 21# tks) (\x -> happyReturn (let {(HappyWrap70 x') = happyOut70 x} in x'))

pRotateAmount tks = happySomeParser where
 happySomeParser = happyThen (happyParse 22# tks) (\x -> happyReturn (let {(HappyWrap71 x') = happyOut71 x} in x'))

pPiecewiseCase tks = happySomeParser where
 happySomeParser = happyThen (happyParse 23# tks) (\x -> happyReturn (let {(HappyWrap72 x') = happyOut72 x} in x'))

pListPiecewiseCase tks = happySomeParser where
 happySomeParser = happyThen (happyParse 24# tks) (\x -> happyReturn (let {(HappyWrap73 x') = happyOut73 x} in x'))

pExp tks = happySomeParser where
 happySomeParser = happyThen (happyParse 25# tks) (\x -> happyReturn (let {(HappyWrap74 x') = happyOut74 x} in x'))

pExp1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 26# tks) (\x -> happyReturn (let {(HappyWrap75 x') = happyOut75 x} in x'))

pExp2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 27# tks) (\x -> happyReturn (let {(HappyWrap76 x') = happyOut76 x} in x'))

pExp3 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 28# tks) (\x -> happyReturn (let {(HappyWrap77 x') = happyOut77 x} in x'))

pExp4 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 29# tks) (\x -> happyReturn (let {(HappyWrap78 x') = happyOut78 x} in x'))

pExp5 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 30# tks) (\x -> happyReturn (let {(HappyWrap79 x') = happyOut79 x} in x'))

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ id(prToken t) ++ "'"

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 18 "<built-in>" #-}
{-# LINE 1 "/Users/Dandoh/.stack/programs/x86_64-osx/ghc-8.6.5/lib/ghc-8.6.5/include/ghcversion.h" #-}
















{-# LINE 19 "<built-in>" #-}
{-# LINE 1 "/var/folders/cw/k0rcq_w96v333f56_9w3yj8h0000gp/T/ghc31112_0/ghc_2.h" #-}

































































































































































































{-# LINE 20 "<built-in>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif

{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList








{-# LINE 65 "templates/GenericTemplate.hs" #-}


{-# LINE 75 "templates/GenericTemplate.hs" #-}










infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}
          

          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}
                                                   
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}
                                     

                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+#  i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)


{-# LINE 180 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+#  nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+#  nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.

