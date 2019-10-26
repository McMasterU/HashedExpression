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

-- parser produced by Happy Version 1.19.12

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
newtype HappyWrap34 = HappyWrap34 (Integer)
happyIn34 :: (Integer) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap34 x)
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> HappyWrap34
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
newtype HappyWrap35 = HappyWrap35 (Double)
happyIn35 :: (Double) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap35 x)
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> HappyWrap35
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
newtype HappyWrap36 = HappyWrap36 (String)
happyIn36 :: (String) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap36 x)
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> HappyWrap36
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
newtype HappyWrap37 = HappyWrap37 (KWDataPattern)
happyIn37 :: (KWDataPattern) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap37 x)
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> HappyWrap37
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
newtype HappyWrap38 = HappyWrap38 (PIdent)
happyIn38 :: (PIdent) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap38 x)
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> HappyWrap38
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
newtype HappyWrap39 = HappyWrap39 (Problem)
happyIn39 :: (Problem) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap39 x)
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> HappyWrap39
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
newtype HappyWrap40 = HappyWrap40 (Block)
happyIn40 :: (Block) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap40 x)
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> HappyWrap40
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
newtype HappyWrap41 = HappyWrap41 ([Block])
happyIn41 :: ([Block]) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap41 x)
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> HappyWrap41
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
newtype HappyWrap42 = HappyWrap42 (Z)
happyIn42 :: (Z) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap42 x)
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> HappyWrap42
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
newtype HappyWrap43 = HappyWrap43 (R)
happyIn43 :: (R) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap43 x)
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> HappyWrap43
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
newtype HappyWrap44 = HappyWrap44 (Number)
happyIn44 :: (Number) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap44 x)
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> HappyWrap44
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
newtype HappyWrap45 = HappyWrap45 (Val)
happyIn45 :: (Val) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap45 x)
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> HappyWrap45
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
newtype HappyWrap46 = HappyWrap46 (Dim)
happyIn46 :: (Dim) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap46 x)
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> HappyWrap46
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
newtype HappyWrap47 = HappyWrap47 (Shape)
happyIn47 :: (Shape) -> (HappyAbsSyn )
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap47 x)
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> HappyWrap47
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
newtype HappyWrap48 = HappyWrap48 (VariableDecl)
happyIn48 :: (VariableDecl) -> (HappyAbsSyn )
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap48 x)
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> HappyWrap48
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
newtype HappyWrap49 = HappyWrap49 ([VariableDecl])
happyIn49 :: ([VariableDecl]) -> (HappyAbsSyn )
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap49 x)
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> HappyWrap49
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
newtype HappyWrap50 = HappyWrap50 ([[VariableDecl]])
happyIn50 :: ([[VariableDecl]]) -> (HappyAbsSyn )
happyIn50 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap50 x)
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> HappyWrap50
happyOut50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut50 #-}
newtype HappyWrap51 = HappyWrap51 (ConstantDecl)
happyIn51 :: (ConstantDecl) -> (HappyAbsSyn )
happyIn51 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap51 x)
{-# INLINE happyIn51 #-}
happyOut51 :: (HappyAbsSyn ) -> HappyWrap51
happyOut51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut51 #-}
newtype HappyWrap52 = HappyWrap52 ([ConstantDecl])
happyIn52 :: ([ConstantDecl]) -> (HappyAbsSyn )
happyIn52 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap52 x)
{-# INLINE happyIn52 #-}
happyOut52 :: (HappyAbsSyn ) -> HappyWrap52
happyOut52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut52 #-}
newtype HappyWrap53 = HappyWrap53 ([[ConstantDecl]])
happyIn53 :: ([[ConstantDecl]]) -> (HappyAbsSyn )
happyIn53 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap53 x)
{-# INLINE happyIn53 #-}
happyOut53 :: (HappyAbsSyn ) -> HappyWrap53
happyOut53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut53 #-}
newtype HappyWrap54 = HappyWrap54 (LetDecl)
happyIn54 :: (LetDecl) -> (HappyAbsSyn )
happyIn54 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap54 x)
{-# INLINE happyIn54 #-}
happyOut54 :: (HappyAbsSyn ) -> HappyWrap54
happyOut54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut54 #-}
newtype HappyWrap55 = HappyWrap55 ([LetDecl])
happyIn55 :: ([LetDecl]) -> (HappyAbsSyn )
happyIn55 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap55 x)
{-# INLINE happyIn55 #-}
happyOut55 :: (HappyAbsSyn ) -> HappyWrap55
happyOut55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut55 #-}
newtype HappyWrap56 = HappyWrap56 ([[LetDecl]])
happyIn56 :: ([[LetDecl]]) -> (HappyAbsSyn )
happyIn56 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap56 x)
{-# INLINE happyIn56 #-}
happyOut56 :: (HappyAbsSyn ) -> HappyWrap56
happyOut56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut56 #-}
newtype HappyWrap57 = HappyWrap57 (Bound)
happyIn57 :: (Bound) -> (HappyAbsSyn )
happyIn57 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap57 x)
{-# INLINE happyIn57 #-}
happyOut57 :: (HappyAbsSyn ) -> HappyWrap57
happyOut57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut57 #-}
newtype HappyWrap58 = HappyWrap58 (ConstraintDecl)
happyIn58 :: (ConstraintDecl) -> (HappyAbsSyn )
happyIn58 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap58 x)
{-# INLINE happyIn58 #-}
happyOut58 :: (HappyAbsSyn ) -> HappyWrap58
happyOut58 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut58 #-}
newtype HappyWrap59 = HappyWrap59 ([ConstraintDecl])
happyIn59 :: ([ConstraintDecl]) -> (HappyAbsSyn )
happyIn59 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap59 x)
{-# INLINE happyIn59 #-}
happyOut59 :: (HappyAbsSyn ) -> HappyWrap59
happyOut59 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut59 #-}
newtype HappyWrap60 = HappyWrap60 ([[ConstraintDecl]])
happyIn60 :: ([[ConstraintDecl]]) -> (HappyAbsSyn )
happyIn60 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap60 x)
{-# INLINE happyIn60 #-}
happyOut60 :: (HappyAbsSyn ) -> HappyWrap60
happyOut60 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut60 #-}
newtype HappyWrap61 = HappyWrap61 (RotateAmount)
happyIn61 :: (RotateAmount) -> (HappyAbsSyn )
happyIn61 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap61 x)
{-# INLINE happyIn61 #-}
happyOut61 :: (HappyAbsSyn ) -> HappyWrap61
happyOut61 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut61 #-}
newtype HappyWrap62 = HappyWrap62 (PiecewiseCase)
happyIn62 :: (PiecewiseCase) -> (HappyAbsSyn )
happyIn62 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap62 x)
{-# INLINE happyIn62 #-}
happyOut62 :: (HappyAbsSyn ) -> HappyWrap62
happyOut62 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut62 #-}
newtype HappyWrap63 = HappyWrap63 ([PiecewiseCase])
happyIn63 :: ([PiecewiseCase]) -> (HappyAbsSyn )
happyIn63 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap63 x)
{-# INLINE happyIn63 #-}
happyOut63 :: (HappyAbsSyn ) -> HappyWrap63
happyOut63 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut63 #-}
newtype HappyWrap64 = HappyWrap64 (Exp)
happyIn64 :: (Exp) -> (HappyAbsSyn )
happyIn64 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap64 x)
{-# INLINE happyIn64 #-}
happyOut64 :: (HappyAbsSyn ) -> HappyWrap64
happyOut64 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut64 #-}
newtype HappyWrap65 = HappyWrap65 (Exp)
happyIn65 :: (Exp) -> (HappyAbsSyn )
happyIn65 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap65 x)
{-# INLINE happyIn65 #-}
happyOut65 :: (HappyAbsSyn ) -> HappyWrap65
happyOut65 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut65 #-}
newtype HappyWrap66 = HappyWrap66 (Exp)
happyIn66 :: (Exp) -> (HappyAbsSyn )
happyIn66 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap66 x)
{-# INLINE happyIn66 #-}
happyOut66 :: (HappyAbsSyn ) -> HappyWrap66
happyOut66 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut66 #-}
newtype HappyWrap67 = HappyWrap67 (Exp)
happyIn67 :: (Exp) -> (HappyAbsSyn )
happyIn67 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap67 x)
{-# INLINE happyIn67 #-}
happyOut67 :: (HappyAbsSyn ) -> HappyWrap67
happyOut67 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut67 #-}
newtype HappyWrap68 = HappyWrap68 (Exp)
happyIn68 :: (Exp) -> (HappyAbsSyn )
happyIn68 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap68 x)
{-# INLINE happyIn68 #-}
happyOut68 :: (HappyAbsSyn ) -> HappyWrap68
happyOut68 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut68 #-}
newtype HappyWrap69 = HappyWrap69 (Exp)
happyIn69 :: (Exp) -> (HappyAbsSyn )
happyIn69 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap69 x)
{-# INLINE happyIn69 #-}
happyOut69 :: (HappyAbsSyn ) -> HappyWrap69
happyOut69 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut69 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe0\xcd\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe0\xcd\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe0\xcd\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\xe0\x01\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x00\x10\x20\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x00\x10\x20\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x00\x10\x20\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x12\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x12\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x00\x10\x20\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x00\x00\x20\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x00\x00\x20\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x00\x00\x20\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x00\x00\x20\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x00\x00\x00\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x00\x10\x20\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x00\x10\x20\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x16\x14\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe0\xcd\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x00\x10\x20\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x00\x00\x20\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x00\x00\x20\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x00\x00\x20\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x00\x10\x20\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x00\x10\x20\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x00\x10\x20\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x12\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x96\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x00\x00\x20\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x00\x00\x20\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x00\x00\x20\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x00\x00\x20\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x00\x00\x00\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\xe0\x01\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\xe0\x01\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x00\x10\x20\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x00\x10\x20\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x00\x10\x20\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x00\x10\x20\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x12\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProblem","%start_pBlock","%start_pListBlock","%start_pZ","%start_pR","%start_pNumber","%start_pVal","%start_pDim","%start_pShape","%start_pVariableDecl","%start_pListVariableDecl","%start_pListListVariableDecl","%start_pConstantDecl","%start_pListConstantDecl","%start_pListListConstantDecl","%start_pLetDecl","%start_pListLetDecl","%start_pListListLetDecl","%start_pBound","%start_pConstraintDecl","%start_pListConstraintDecl","%start_pListListConstraintDecl","%start_pRotateAmount","%start_pPiecewiseCase","%start_pListPiecewiseCase","%start_pExp","%start_pExp1","%start_pExp2","%start_pExp3","%start_pExp4","%start_pExp5","Integer","Double","String","KWDataPattern","PIdent","Problem","Block","ListBlock","Z","R","Number","Val","Dim","Shape","VariableDecl","ListVariableDecl","ListListVariableDecl","ConstantDecl","ListConstantDecl","ListListConstantDecl","LetDecl","ListLetDecl","ListListLetDecl","Bound","ConstraintDecl","ListConstraintDecl","ListListConstraintDecl","RotateAmount","PiecewiseCase","ListPiecewiseCase","Exp","Exp1","Exp2","Exp3","Exp4","Exp5","'('","')'","'*'","'*.'","'+'","'+:'","','","'-'","'->'","'/'","':'","';'","'<.>'","'<='","'='","'>='","'Dataset'","'File'","'Pattern'","'Random'","'['","']'","'^'","'case'","'constant'","'constants'","'constraint'","'constraints'","'it'","'let'","'minimize'","'otherwise'","'rotate'","'variable'","'variables'","'{'","'}'","L_integ","L_doubl","L_quoted","L_KWDataPattern","L_PIdent","%eof"]
        bit_start = st * 112
        bit_end = (st + 1) * 112
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..111]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\xa2\x02\xa2\x02\xa2\x02\xff\xff\xf9\xff\x5c\x00\x0d\x02\xee\xff\xee\xff\xf4\xff\xf4\xff\xa9\x00\xf4\xff\xf4\xff\x1e\x01\xf4\xff\xf4\xff\x42\x01\xcd\x00\x19\x00\x19\x00\x03\x00\x22\x00\xf2\xff\xf2\xff\x19\x00\x0f\x00\x0f\x00\x0f\x00\x0f\x00\x0d\x00\x36\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x33\x00\x19\x00\x49\x01\x00\x00\x00\x00\x0d\x00\x33\x00\x00\x00\x22\x00\xf1\xff\x00\x00\x10\x00\x51\x00\x07\x00\xaf\x00\x3f\x00\x09\x00\x19\x00\x82\x00\x85\x00\xa4\x00\xb5\x00\x8d\x00\x00\x00\x8d\x00\x8e\x00\xbd\x00\xbb\x00\xb4\x00\xa1\x01\xb4\x00\xb4\x00\x00\x00\x00\x00\xb4\x00\xd3\x00\xe3\x00\xf6\x00\xe6\x00\xe6\x00\xe6\x00\xfe\x00\x13\x01\x1c\x01\x29\x01\x29\x01\x29\x01\x2b\x01\x37\x01\x65\x01\x38\x01\x38\x01\x38\x01\x67\x01\x85\x01\x6c\x01\x8c\x01\x00\x00\x8c\x01\x9f\x01\xc1\x01\xce\x01\x00\x00\xaf\x01\xaf\x01\xc9\x01\xb7\x01\xbd\x01\xa2\x02\xc4\x01\xe6\x01\xee\x01\xef\x01\xf2\x01\xf3\x01\xf9\x01\xfa\x01\xfc\x01\xd1\x01\xd1\x01\x00\x00\xf0\x01\x04\x02\x05\x02\x07\x02\x08\x02\x09\x02\x12\x02\x13\x02\x00\x00\x00\x00\x00\x00\xdf\x01\xea\x01\xea\x01\x22\x02\x27\x02\xbb\x01\x14\x02\x0e\x02\xe4\x01\x15\x02\x2e\x02\xf1\x01\x16\x02\x19\x00\x0f\x00\x0f\x00\x0f\x00\xcd\x00\xcd\x00\x01\x00\x19\x00\x3a\x02\x19\x00\x5c\x00\xf2\xff\xd9\x01\x0f\x00\x0f\x00\x0f\x00\x0f\x00\x1c\x02\x0d\x00\x00\x00\x0b\x02\x00\x00\x00\x00\x00\x00\x2c\x02\x2c\x02\xaf\x00\xaf\x00\x20\x02\x00\x00\x3c\x02\x4d\x01\x24\x02\x00\x00\x00\x00\x00\x00\x00\x00\x09\x00\x09\x00\x09\x00\x4d\x01\x00\x00\x00\x00\x0d\x02\x00\x00\x00\x00\x0d\x02\x00\x00\x00\x00\x00\x00\x00\x00\x44\x02\x00\x00\x4a\x02\x4e\x02\x00\x00\x66\x00\x66\x00\x17\x00\x17\x00\x18\x01\x19\x00\x2c\x01\x2c\x01\x2a\x02\x35\x02\x44\x00\x36\x02\x38\x02\x39\x02\x40\x02\x41\x02\x00\x00\x00\x00\x37\x02\x00\x00\x00\x00\x20\x01\x19\x00\xf2\xff\x43\x02\x4d\x01\x00\x00\x4b\x02\x67\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x68\x02\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xe7\x01\x6c\x02\x71\x01\x05\x00\x83\x00\xaa\x01\x57\x01\x6a\x02\x74\x01\x42\x00\x71\x00\x0e\x01\x47\x00\xd2\x00\xb7\x00\x02\x00\x4b\x00\x3e\x00\x95\x02\xdd\x00\xad\x00\x4d\x00\x5b\x00\x57\x02\x7f\x01\xe8\x00\x99\x01\xf7\x01\x31\x02\x63\x02\x74\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x01\x9e\x01\x00\x00\x00\x00\x7f\x02\x00\x00\x00\x00\x74\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x17\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x77\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb4\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbe\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x75\x02\x00\x00\x82\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x83\x02\x00\x00\x8c\x02\xc7\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8d\x02\x8b\x02\x8f\x02\x00\x00\x8e\x02\x15\x01\x25\x01\x00\x00\xdc\x00\xfd\x00\x00\x00\x84\x00\xab\x00\x3b\x01\xa4\x01\xc8\x01\xd3\x01\xa0\x02\xb0\x02\x59\x00\xb9\x00\x00\x00\x46\x01\xb5\x01\xb9\x01\x00\x00\x02\x02\x26\x02\x4d\x02\x58\x02\x98\x02\x8a\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9b\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7a\x01\x00\x00\x00\x00\x86\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe7\x00\xf2\x00\x7d\x00\x89\x00\x90\x00\x6a\x01\x22\x01\x3d\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9a\x02\x00\x00\x00\x00\x00\x00\x75\x01\xbc\x01\x00\x00\x00\x00\x00\x00\xa3\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc4\xff\x00\x00\xbe\xff\xbe\xff\x00\x00\xb7\xff\xb7\xff\x00\x00\xb0\xff\xb0\xff\x00\x00\x00\x00\xa6\xff\xa6\xff\x00\x00\x00\x00\x9b\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe0\xff\xd0\xff\xce\xff\x86\xff\xcc\xff\xcb\xff\x87\xff\x00\x00\x00\x00\x00\x00\xdf\xff\xdc\xff\x86\xff\x00\x00\x89\xff\x00\x00\x00\x00\x8c\xff\x00\x00\x8e\xff\x00\x00\x91\xff\x00\x00\x95\xff\x00\x00\x9a\xff\x00\x00\x00\x00\x00\x00\x00\x00\xa0\xff\x00\x00\x00\x00\xa5\xff\xa2\xff\x00\x00\x00\x00\x00\x00\x00\x00\xaa\xff\xa9\xff\x00\x00\x00\x00\xaf\xff\xac\xff\x00\x00\x00\x00\x00\x00\xc4\xff\xb6\xff\xb3\xff\x00\x00\x00\x00\x00\x00\xc4\xff\xbd\xff\xba\xff\x00\x00\x00\x00\x00\x00\xc3\xff\x00\x00\x00\x00\x00\x00\xc6\xff\x00\x00\x00\x00\x00\x00\x00\x00\xc7\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd2\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdb\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd1\xff\xcf\xff\xcd\xff\x00\x00\x00\x00\x00\x00\x00\x00\xc2\xff\xbe\xff\xbe\xff\xc0\xff\xb7\xff\xb7\xff\x00\x00\xb0\xff\xb0\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa6\xff\xa6\xff\x00\x00\x00\x00\x00\x00\x9b\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8b\xff\x00\x00\x88\xff\x8a\xff\x8d\xff\x8f\xff\x90\xff\x92\xff\x93\xff\x00\x00\x99\xff\x00\x00\x9c\xff\x00\x00\xa4\xff\xa1\xff\xa8\xff\xa7\xff\x96\xff\x97\xff\x98\xff\xb1\xff\xae\xff\xab\xff\x00\x00\xb5\xff\xb2\xff\x00\x00\xbc\xff\xb9\xff\xc1\xff\xc5\xff\x00\x00\xde\xff\x00\x00\x00\x00\xdd\xff\xb7\xff\xb7\xff\xa6\xff\xa6\xff\xb0\xff\x00\x00\xbe\xff\xbe\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc8\xff\xca\xff\x00\x00\xbf\xff\xb8\xff\x00\x00\x00\x00\x9b\xff\x00\x00\x9d\xff\x9f\xff\x00\x00\x00\x00\xd7\xff\xd8\xff\xd5\xff\xd6\xff\xd4\xff\xd3\xff\xd9\xff\xda\xff\xc9\xff\x00\x00\x94\xff\x9e\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x08\x00\x01\x00\x15\x00\x01\x00\x00\x00\x04\x00\x08\x00\x17\x00\x08\x00\x03\x00\x08\x00\x03\x00\x08\x00\x01\x00\x1d\x00\x01\x00\x0a\x00\x20\x00\x0a\x00\x04\x00\x08\x00\x14\x00\x08\x00\x01\x00\x18\x00\x01\x00\x18\x00\x2b\x00\x0d\x00\x2a\x00\x08\x00\x27\x00\x08\x00\x21\x00\x01\x00\x21\x00\x26\x00\x25\x00\x26\x00\x27\x00\x26\x00\x27\x00\x2a\x00\x2b\x00\x2a\x00\x2b\x00\x18\x00\x21\x00\x18\x00\x2b\x00\x26\x00\x27\x00\x26\x00\x27\x00\x2a\x00\x21\x00\x2a\x00\x21\x00\x2b\x00\x25\x00\x26\x00\x27\x00\x26\x00\x27\x00\x2a\x00\x04\x00\x2a\x00\x05\x00\x06\x00\x04\x00\x08\x00\x26\x00\x05\x00\x06\x00\x04\x00\x08\x00\x00\x00\x01\x00\x04\x00\x0e\x00\x04\x00\x14\x00\x15\x00\x16\x00\x08\x00\x09\x00\x0a\x00\x11\x00\x00\x00\x01\x00\x00\x00\x26\x00\x04\x00\x2b\x00\x14\x00\x15\x00\x08\x00\x09\x00\x0a\x00\x08\x00\x18\x00\x19\x00\x1a\x00\x17\x00\x25\x00\x2b\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x04\x00\x1b\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x00\x00\x01\x00\x0e\x00\x0f\x00\x04\x00\x26\x00\x27\x00\x01\x00\x08\x00\x09\x00\x0a\x00\x04\x00\x00\x00\x01\x00\x25\x00\x09\x00\x04\x00\x0c\x00\x1b\x00\x2a\x00\x08\x00\x09\x00\x0a\x00\x04\x00\x18\x00\x19\x00\x1a\x00\x14\x00\x15\x00\x16\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x18\x00\x19\x00\x1a\x00\x14\x00\x15\x00\x16\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x00\x00\x01\x00\x04\x00\x2b\x00\x04\x00\x0e\x00\x04\x00\x26\x00\x08\x00\x09\x00\x0a\x00\x2b\x00\x00\x00\x01\x00\x04\x00\x0d\x00\x04\x00\x09\x00\x14\x00\x15\x00\x08\x00\x09\x00\x0a\x00\x07\x00\x18\x00\x19\x00\x0c\x00\x11\x00\x12\x00\x13\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x18\x00\x19\x00\x2a\x00\x2b\x00\x08\x00\x04\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x00\x00\x01\x00\x2b\x00\x04\x00\x04\x00\x0f\x00\x11\x00\x12\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x01\x00\x07\x00\x04\x00\x04\x00\x11\x00\x12\x00\x13\x00\x08\x00\x09\x00\x0a\x00\x26\x00\x27\x00\x18\x00\x04\x00\x2a\x00\x11\x00\x12\x00\x13\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x04\x00\x0c\x00\x11\x00\x12\x00\x13\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x00\x00\x01\x00\x11\x00\x12\x00\x04\x00\x2b\x00\x04\x00\x15\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x01\x00\x04\x00\x07\x00\x04\x00\x0e\x00\x0f\x00\x10\x00\x08\x00\x09\x00\x0a\x00\x02\x00\x0e\x00\x0f\x00\x10\x00\x04\x00\x07\x00\x0c\x00\x04\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x0e\x00\x0f\x00\x10\x00\x0e\x00\x0f\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x00\x00\x01\x00\x25\x00\x07\x00\x04\x00\x15\x00\x04\x00\x2a\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x01\x00\x2a\x00\x2b\x00\x04\x00\x0e\x00\x0f\x00\x10\x00\x08\x00\x09\x00\x0a\x00\x25\x00\x05\x00\x06\x00\x2b\x00\x08\x00\x2a\x00\x00\x00\x01\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x2b\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x00\x00\x01\x00\x2a\x00\x2b\x00\x04\x00\x26\x00\x27\x00\x0c\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x01\x00\x06\x00\x07\x00\x04\x00\x00\x00\x01\x00\x15\x00\x08\x00\x09\x00\x0a\x00\x0c\x00\x0d\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x00\x00\x01\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x26\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x00\x00\x01\x00\x1c\x00\x1d\x00\x04\x00\x00\x00\x01\x00\x01\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x01\x00\x05\x00\x06\x00\x04\x00\x08\x00\x00\x00\x01\x00\x08\x00\x09\x00\x0a\x00\x0e\x00\x2b\x00\x10\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x01\x00\x2b\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x08\x00\x09\x00\x0a\x00\x0c\x00\x0d\x00\x01\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x00\x00\x01\x00\x0c\x00\x0d\x00\x04\x00\x06\x00\x07\x00\x01\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x01\x00\x1c\x00\x1d\x00\x04\x00\x1c\x00\x1d\x00\x2b\x00\x08\x00\x09\x00\x0a\x00\x05\x00\x06\x00\x25\x00\x08\x00\x2b\x00\x26\x00\x0b\x00\x2a\x00\x2b\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x05\x00\x06\x00\x07\x00\x2b\x00\x27\x00\x0b\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x00\x00\x01\x00\x0b\x00\x0b\x00\x04\x00\x2b\x00\x0b\x00\x0b\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x01\x00\x0b\x00\x0b\x00\x04\x00\x0b\x00\x29\x00\x25\x00\x08\x00\x09\x00\x0a\x00\x02\x00\x2a\x00\x2b\x00\x05\x00\x06\x00\x28\x00\x08\x00\x24\x00\x08\x00\x25\x00\x20\x00\x21\x00\x22\x00\x23\x00\x2a\x00\x2b\x00\x0f\x00\x11\x00\x12\x00\x13\x00\x14\x00\x20\x00\x21\x00\x22\x00\x23\x00\x00\x00\x01\x00\x24\x00\x24\x00\x04\x00\x24\x00\x24\x00\x24\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x01\x00\x26\x00\x27\x00\x04\x00\x24\x00\x24\x00\x16\x00\x08\x00\x09\x00\x0a\x00\x15\x00\x0f\x00\x2a\x00\x2a\x00\x2a\x00\x07\x00\x26\x00\x17\x00\x24\x00\x09\x00\x20\x00\x21\x00\x22\x00\x23\x00\x26\x00\x07\x00\x02\x00\x00\x00\x01\x00\x25\x00\x02\x00\x04\x00\x21\x00\x22\x00\x23\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x01\x00\x25\x00\x25\x00\x04\x00\x25\x00\x25\x00\x28\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x01\x00\x25\x00\x25\x00\x04\x00\x25\x00\x02\x00\x02\x00\x08\x00\x09\x00\x0a\x00\x21\x00\x22\x00\x23\x00\x26\x00\x06\x00\x1c\x00\x00\x00\x01\x00\x0c\x00\x00\x00\x04\x00\x21\x00\x22\x00\x23\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x01\x00\x0c\x00\x00\x00\x04\x00\x01\x00\x22\x00\x23\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x01\x00\x00\x00\x02\x00\x04\x00\xff\xff\x03\x00\x02\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x01\x00\x23\x00\x00\x00\x04\x00\x0c\x00\x00\x00\x02\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x01\x00\x23\x00\x00\x00\x04\x00\xff\xff\xff\xff\xff\xff\x08\x00\x09\x00\x0a\x00\xff\xff\x17\x00\x23\x00\xff\xff\xff\xff\x00\x00\x01\x00\xff\xff\xff\xff\x04\x00\xff\xff\xff\xff\x17\x00\x08\x00\x09\x00\x0a\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\xff\xff\x1e\x00\x1f\x00\xff\xff\xff\xff\x22\x00\x23\x00\xff\xff\x17\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x69\x00\x29\x00\x5f\x00\x29\x00\x21\x00\x4a\x00\x6b\x00\xa2\x00\x2a\x00\x9e\x00\x2a\x00\x9e\x00\x69\x00\x29\x00\x3c\x00\x29\x00\x9f\x00\x3d\x00\x9f\x00\xa0\x00\x2a\x00\x4f\x00\x2a\x00\x29\x00\x39\x00\x29\x00\x39\x00\xff\xff\xa1\x00\x2c\x00\x2a\x00\x2b\x00\x2a\x00\x30\x00\x41\x00\x30\x00\x21\x00\xa3\xff\x21\x00\x2b\x00\x21\x00\x2b\x00\x2c\x00\xa3\xff\x2c\x00\xa3\xff\x39\x00\x30\x00\x39\x00\xff\xff\x21\x00\x2b\x00\x21\x00\x2b\x00\x2c\x00\x30\x00\x2c\x00\x30\x00\xff\xff\xa3\xff\x21\x00\x2b\x00\x21\x00\x2b\x00\x2c\x00\x4a\x00\x2c\x00\x92\x00\x93\x00\x56\x00\x94\x00\x21\x00\x92\x00\x93\x00\x50\x00\x94\x00\x21\x00\x22\x00\x4a\x00\x5b\x00\x2c\x00\x4b\x00\x4c\x00\x4d\x00\x24\x00\x25\x00\x26\x00\x55\x00\x21\x00\x22\x00\x3e\x00\x21\x00\x2c\x00\xff\xff\x4b\x00\x4e\x00\x24\x00\x25\x00\x26\x00\x2a\x00\x41\x00\x42\x00\x43\x00\xa2\x00\xeb\x00\xff\xff\x44\x00\x37\x00\x35\x00\x33\x00\x31\x00\x2e\x00\x41\x00\x42\x00\xb2\x00\x3e\x00\x56\x00\x3f\x00\x44\x00\x37\x00\x35\x00\x33\x00\x31\x00\x2e\x00\x21\x00\x22\x00\x57\x00\x5a\x00\x2c\x00\x21\x00\x2b\x00\x22\x00\x24\x00\x25\x00\x26\x00\x4a\x00\x21\x00\x22\x00\xb4\xff\x67\x00\x2c\x00\x9c\x00\xa2\x00\x2c\x00\x24\x00\x25\x00\x26\x00\x4a\x00\x41\x00\x42\x00\xd5\x00\x4b\x00\x4c\x00\xba\x00\x44\x00\x37\x00\x35\x00\x33\x00\x31\x00\x2e\x00\x41\x00\x42\x00\xd4\x00\x4b\x00\x4c\x00\xd3\x00\x44\x00\x37\x00\x35\x00\x33\x00\x31\x00\x2e\x00\x21\x00\x22\x00\x4a\x00\xff\xff\x2c\x00\x9b\x00\xa0\x00\x21\x00\x24\x00\x25\x00\x26\x00\xff\xff\x21\x00\x22\x00\x50\x00\xa1\x00\x2c\x00\x9a\x00\x4b\x00\xb9\x00\x24\x00\x25\x00\x26\x00\x98\x00\x41\x00\x45\x00\x97\x00\x51\x00\x52\x00\x53\x00\x44\x00\x37\x00\x35\x00\x33\x00\x31\x00\x2e\x00\x41\x00\xb1\x00\x2c\x00\xbb\xff\x2a\x00\x50\x00\x44\x00\x37\x00\x35\x00\x33\x00\x31\x00\x2e\x00\x21\x00\x22\x00\xff\xff\x50\x00\x2c\x00\x91\x00\x51\x00\x54\x00\x24\x00\x25\x00\x26\x00\x21\x00\x22\x00\x90\x00\x50\x00\x2c\x00\x51\x00\x52\x00\xbd\x00\x24\x00\x25\x00\x26\x00\x21\x00\x2b\x00\x46\x00\x50\x00\x2c\x00\x51\x00\x52\x00\xd7\x00\x44\x00\x37\x00\x35\x00\x33\x00\x31\x00\x2e\x00\x50\x00\x8f\x00\x51\x00\x52\x00\xd6\x00\x36\x00\x37\x00\x35\x00\x33\x00\x31\x00\x2e\x00\x21\x00\x22\x00\x51\x00\xbc\x00\x2c\x00\xff\xff\x56\x00\x5f\x00\x24\x00\x25\x00\x26\x00\x21\x00\x22\x00\x56\x00\x8d\x00\x2c\x00\x57\x00\x58\x00\x59\x00\x24\x00\x25\x00\x26\x00\xe3\x00\x57\x00\x58\x00\xc0\x00\x56\x00\xe4\x00\x8c\x00\x56\x00\xa4\x00\x37\x00\x35\x00\x33\x00\x31\x00\x2e\x00\x57\x00\x58\x00\xd1\x00\x57\x00\xbf\x00\x9c\x00\x37\x00\x35\x00\x33\x00\x31\x00\x2e\x00\x21\x00\x22\x00\xad\xff\x8a\x00\x2c\x00\x5f\x00\x56\x00\x2c\x00\x24\x00\x25\x00\x26\x00\x21\x00\x22\x00\x2c\x00\xb4\xff\x2c\x00\x57\x00\x58\x00\xd0\x00\x24\x00\x25\x00\x26\x00\xbb\xff\x92\x00\x93\x00\xff\xff\x94\x00\x2c\x00\x21\x00\x22\x00\xb8\x00\x37\x00\x35\x00\x33\x00\x31\x00\x2e\x00\x24\x00\x25\x00\x60\x00\x61\x00\xff\xff\xaf\x00\x37\x00\x35\x00\x33\x00\x31\x00\x2e\x00\x21\x00\x22\x00\x2c\x00\xad\xff\x2c\x00\x21\x00\x2b\x00\x89\x00\x24\x00\x25\x00\x26\x00\x21\x00\x22\x00\x6b\x00\x6c\x00\x2c\x00\x21\x00\x22\x00\x5f\x00\x24\x00\x25\x00\x26\x00\x5c\x00\x5d\x00\x24\x00\x25\x00\x60\x00\xdc\x00\x21\x00\x22\x00\xd2\x00\x37\x00\x35\x00\x33\x00\x31\x00\x2e\x00\x24\x00\x25\x00\x60\x00\xdb\x00\x21\x00\xe1\x00\x37\x00\x35\x00\x33\x00\x31\x00\x2e\x00\x21\x00\x22\x00\x39\x00\x3a\x00\x2c\x00\x81\x00\x82\x00\x86\x00\x24\x00\x25\x00\x26\x00\x21\x00\x22\x00\x92\x00\x93\x00\x2c\x00\x94\x00\x21\x00\x22\x00\x24\x00\x25\x00\x26\x00\x95\x00\xff\xff\x96\x00\x24\x00\x25\x00\x66\x00\x21\x00\x22\x00\xff\xff\x34\x00\x35\x00\x33\x00\x31\x00\x2e\x00\x24\x00\x25\x00\xae\x00\x5c\x00\x8d\x00\x85\x00\xb7\x00\x35\x00\x33\x00\x31\x00\x2e\x00\x21\x00\x22\x00\x5c\x00\x8a\x00\x2c\x00\x6b\x00\x80\x00\x84\x00\x24\x00\x25\x00\x26\x00\x21\x00\x22\x00\x39\x00\xad\x00\x2c\x00\x39\x00\xe0\x00\xff\xff\x24\x00\x25\x00\x26\x00\x92\x00\x93\x00\xbb\xff\x94\x00\xff\xff\x21\x00\xad\x00\x2c\x00\xbb\xff\xb6\x00\x35\x00\x33\x00\x31\x00\x2e\x00\x76\x00\x6b\x00\x77\x00\xff\xff\x2b\x00\x80\x00\xb5\x00\x35\x00\x33\x00\x31\x00\x2e\x00\x21\x00\x22\x00\x7f\x00\x7e\x00\x2c\x00\xff\xff\x7d\x00\x7c\x00\x24\x00\x25\x00\x26\x00\x21\x00\x22\x00\x7b\x00\x7a\x00\x2c\x00\x79\x00\xc8\x00\xb4\xff\x24\x00\x25\x00\x26\x00\xa6\x00\x2c\x00\xb4\xff\x92\x00\x93\x00\xc5\x00\x94\x00\xd0\x00\x2a\x00\xad\xff\x32\x00\x33\x00\x31\x00\x2e\x00\x2c\x00\xad\xff\xbf\x00\x63\x00\x64\x00\x65\x00\x66\x00\xab\x00\x33\x00\x31\x00\x2e\x00\x21\x00\x22\x00\xcf\x00\xce\x00\x2c\x00\xcd\x00\xcc\x00\xcb\x00\x24\x00\x25\x00\x26\x00\x21\x00\x22\x00\x21\x00\x2b\x00\x2c\x00\xca\x00\xc9\x00\xc3\x00\x24\x00\x25\x00\x26\x00\x5f\x00\xbc\x00\x2c\x00\x2c\x00\x2c\x00\xb1\x00\x21\x00\xa2\x00\xe0\x00\xdf\x00\xaa\x00\x33\x00\x31\x00\x2e\x00\x21\x00\xdb\x00\xda\x00\x21\x00\x22\x00\xed\x00\xd9\x00\x2c\x00\x30\x00\x31\x00\x2e\x00\x24\x00\x25\x00\x26\x00\x21\x00\x22\x00\xec\x00\xea\x00\x2c\x00\xe9\x00\xe8\x00\xc5\x00\x24\x00\x25\x00\x26\x00\x21\x00\x22\x00\xe7\x00\xe6\x00\x2c\x00\xf0\x00\xee\x00\xf1\x00\x24\x00\x25\x00\x26\x00\xa9\x00\x31\x00\x2e\x00\x21\x00\x75\x00\x3d\x00\x21\x00\x22\x00\x5f\x00\x98\x00\x23\x00\xa8\x00\x31\x00\x2e\x00\x24\x00\x25\x00\x26\x00\x21\x00\x22\x00\x87\x00\x86\x00\x23\x00\x82\x00\x2d\x00\x2e\x00\x24\x00\x25\x00\x26\x00\x21\x00\x22\x00\x81\x00\xc5\x00\x23\x00\x00\x00\xc6\x00\xc3\x00\x24\x00\x25\x00\x26\x00\x21\x00\x22\x00\x27\x00\xa7\x00\x47\x00\xc1\x00\xdd\x00\xe4\x00\x24\x00\x25\x00\x48\x00\x21\x00\x22\x00\xa3\x00\xee\x00\x47\x00\x00\x00\x00\x00\x00\x00\x24\x00\x25\x00\x48\x00\x00\x00\x49\x00\xa6\x00\x00\x00\x00\x00\x21\x00\x22\x00\x00\x00\x00\x00\x47\x00\x00\x00\x00\x00\xb4\x00\x24\x00\x25\x00\x48\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x00\x00\x72\x00\x73\x00\x00\x00\x00\x00\x74\x00\x75\x00\x00\x00\xb3\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (31, 121) [
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
	(121 , happyReduce_121)
	]

happy_n_terms = 44 :: Int
happy_n_nonterms = 36 :: Int

happyReduce_31 = happySpecReduce_1  0# happyReduction_31
happyReduction_31 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TI happy_var_1)) -> 
	happyIn34
		 ((read ( happy_var_1)) :: Integer
	)}

happyReduce_32 = happySpecReduce_1  1# happyReduction_32
happyReduction_32 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TD happy_var_1)) -> 
	happyIn35
		 ((read ( happy_var_1)) :: Double
	)}

happyReduce_33 = happySpecReduce_1  2# happyReduction_33
happyReduction_33 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TL happy_var_1)) -> 
	happyIn36
		 (happy_var_1
	)}

happyReduce_34 = happySpecReduce_1  3# happyReduction_34
happyReduction_34 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (T_KWDataPattern happy_var_1)) -> 
	happyIn37
		 (KWDataPattern (happy_var_1)
	)}

happyReduce_35 = happySpecReduce_1  4# happyReduction_35
happyReduction_35 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn38
		 (PIdent (mkPosToken happy_var_1)
	)}

happyReduce_36 = happySpecReduce_1  5# happyReduction_36
happyReduction_36 happy_x_1
	 =  case happyOut41 happy_x_1 of { (HappyWrap41 happy_var_1) -> 
	happyIn39
		 (AbsHashedLang.Problem happy_var_1
	)}

happyReduce_37 = happyReduce 5# 6# happyReduction_37
happyReduction_37 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut50 happy_x_4 of { (HappyWrap50 happy_var_4) -> 
	happyIn40
		 (AbsHashedLang.BlockVariable happy_var_4
	) `HappyStk` happyRest}

happyReduce_38 = happyReduce 5# 6# happyReduction_38
happyReduction_38 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut50 happy_x_4 of { (HappyWrap50 happy_var_4) -> 
	happyIn40
		 (AbsHashedLang.BlockVariable happy_var_4
	) `HappyStk` happyRest}

happyReduce_39 = happyReduce 5# 6# happyReduction_39
happyReduction_39 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut53 happy_x_4 of { (HappyWrap53 happy_var_4) -> 
	happyIn40
		 (AbsHashedLang.BlockConstant happy_var_4
	) `HappyStk` happyRest}

happyReduce_40 = happyReduce 5# 6# happyReduction_40
happyReduction_40 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut53 happy_x_4 of { (HappyWrap53 happy_var_4) -> 
	happyIn40
		 (AbsHashedLang.BlockConstant happy_var_4
	) `HappyStk` happyRest}

happyReduce_41 = happyReduce 5# 6# happyReduction_41
happyReduction_41 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut60 happy_x_4 of { (HappyWrap60 happy_var_4) -> 
	happyIn40
		 (AbsHashedLang.BlockConstraint happy_var_4
	) `HappyStk` happyRest}

happyReduce_42 = happyReduce 5# 6# happyReduction_42
happyReduction_42 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut60 happy_x_4 of { (HappyWrap60 happy_var_4) -> 
	happyIn40
		 (AbsHashedLang.BlockConstraint happy_var_4
	) `HappyStk` happyRest}

happyReduce_43 = happyReduce 5# 6# happyReduction_43
happyReduction_43 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut56 happy_x_4 of { (HappyWrap56 happy_var_4) -> 
	happyIn40
		 (AbsHashedLang.BlockLet happy_var_4
	) `HappyStk` happyRest}

happyReduce_44 = happyReduce 5# 6# happyReduction_44
happyReduction_44 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut64 happy_x_4 of { (HappyWrap64 happy_var_4) -> 
	happyIn40
		 (AbsHashedLang.BlockMinimize happy_var_4
	) `HappyStk` happyRest}

happyReduce_45 = happySpecReduce_1  7# happyReduction_45
happyReduction_45 happy_x_1
	 =  case happyOut40 happy_x_1 of { (HappyWrap40 happy_var_1) -> 
	happyIn41
		 ((:[]) happy_var_1
	)}

happyReduce_46 = happySpecReduce_2  7# happyReduction_46
happyReduction_46 happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_1 of { (HappyWrap40 happy_var_1) -> 
	case happyOut41 happy_x_2 of { (HappyWrap41 happy_var_2) -> 
	happyIn41
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_47 = happySpecReduce_1  8# happyReduction_47
happyReduction_47 happy_x_1
	 =  case happyOut34 happy_x_1 of { (HappyWrap34 happy_var_1) -> 
	happyIn42
		 (AbsHashedLang.Z happy_var_1
	)}

happyReduce_48 = happySpecReduce_2  8# happyReduction_48
happyReduction_48 happy_x_2
	happy_x_1
	 =  case happyOut34 happy_x_2 of { (HappyWrap34 happy_var_2) -> 
	happyIn42
		 (AbsHashedLang.Z happy_var_2
	)}

happyReduce_49 = happySpecReduce_1  9# happyReduction_49
happyReduction_49 happy_x_1
	 =  case happyOut35 happy_x_1 of { (HappyWrap35 happy_var_1) -> 
	happyIn43
		 (AbsHashedLang.R happy_var_1
	)}

happyReduce_50 = happySpecReduce_2  9# happyReduction_50
happyReduction_50 happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_2 of { (HappyWrap35 happy_var_2) -> 
	happyIn43
		 (AbsHashedLang.R happy_var_2
	)}

happyReduce_51 = happySpecReduce_1  10# happyReduction_51
happyReduction_51 happy_x_1
	 =  case happyOut42 happy_x_1 of { (HappyWrap42 happy_var_1) -> 
	happyIn44
		 (AbsHashedLang.NumInt happy_var_1
	)}

happyReduce_52 = happySpecReduce_1  10# happyReduction_52
happyReduction_52 happy_x_1
	 =  case happyOut43 happy_x_1 of { (HappyWrap43 happy_var_1) -> 
	happyIn44
		 (AbsHashedLang.NumDouble happy_var_1
	)}

happyReduce_53 = happyReduce 4# 11# happyReduction_53
happyReduction_53 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut36 happy_x_3 of { (HappyWrap36 happy_var_3) -> 
	happyIn45
		 (AbsHashedLang.ValFile happy_var_3
	) `HappyStk` happyRest}

happyReduce_54 = happyReduce 6# 11# happyReduction_54
happyReduction_54 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut36 happy_x_3 of { (HappyWrap36 happy_var_3) -> 
	case happyOut36 happy_x_5 of { (HappyWrap36 happy_var_5) -> 
	happyIn45
		 (AbsHashedLang.ValDataset happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_55 = happyReduce 4# 11# happyReduction_55
happyReduction_55 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut37 happy_x_3 of { (HappyWrap37 happy_var_3) -> 
	happyIn45
		 (AbsHashedLang.ValPattern happy_var_3
	) `HappyStk` happyRest}

happyReduce_56 = happySpecReduce_1  11# happyReduction_56
happyReduction_56 happy_x_1
	 =  happyIn45
		 (AbsHashedLang.ValRandom
	)

happyReduce_57 = happySpecReduce_1  11# happyReduction_57
happyReduction_57 happy_x_1
	 =  case happyOut44 happy_x_1 of { (HappyWrap44 happy_var_1) -> 
	happyIn45
		 (AbsHashedLang.ValLiteral happy_var_1
	)}

happyReduce_58 = happySpecReduce_3  12# happyReduction_58
happyReduction_58 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut34 happy_x_2 of { (HappyWrap34 happy_var_2) -> 
	happyIn46
		 (AbsHashedLang.Dim happy_var_2
	)}

happyReduce_59 = happySpecReduce_0  13# happyReduction_59
happyReduction_59  =  happyIn47
		 (AbsHashedLang.ShapeScalar
	)

happyReduce_60 = happySpecReduce_1  13# happyReduction_60
happyReduction_60 happy_x_1
	 =  case happyOut46 happy_x_1 of { (HappyWrap46 happy_var_1) -> 
	happyIn47
		 (AbsHashedLang.Shape1D happy_var_1
	)}

happyReduce_61 = happySpecReduce_2  13# happyReduction_61
happyReduction_61 happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_1 of { (HappyWrap46 happy_var_1) -> 
	case happyOut46 happy_x_2 of { (HappyWrap46 happy_var_2) -> 
	happyIn47
		 (AbsHashedLang.Shape2D happy_var_1 happy_var_2
	)}}

happyReduce_62 = happySpecReduce_3  13# happyReduction_62
happyReduction_62 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_1 of { (HappyWrap46 happy_var_1) -> 
	case happyOut46 happy_x_2 of { (HappyWrap46 happy_var_2) -> 
	case happyOut46 happy_x_3 of { (HappyWrap46 happy_var_3) -> 
	happyIn47
		 (AbsHashedLang.Shape3D happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_63 = happySpecReduce_2  14# happyReduction_63
happyReduction_63 happy_x_2
	happy_x_1
	 =  case happyOut38 happy_x_1 of { (HappyWrap38 happy_var_1) -> 
	case happyOut47 happy_x_2 of { (HappyWrap47 happy_var_2) -> 
	happyIn48
		 (AbsHashedLang.VariableNoInit happy_var_1 happy_var_2
	)}}

happyReduce_64 = happyReduce 4# 14# happyReduction_64
happyReduction_64 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut38 happy_x_1 of { (HappyWrap38 happy_var_1) -> 
	case happyOut47 happy_x_2 of { (HappyWrap47 happy_var_2) -> 
	case happyOut45 happy_x_4 of { (HappyWrap45 happy_var_4) -> 
	happyIn48
		 (AbsHashedLang.VariableWithInit happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_65 = happySpecReduce_0  15# happyReduction_65
happyReduction_65  =  happyIn49
		 ([]
	)

happyReduce_66 = happySpecReduce_1  15# happyReduction_66
happyReduction_66 happy_x_1
	 =  case happyOut48 happy_x_1 of { (HappyWrap48 happy_var_1) -> 
	happyIn49
		 ((:[]) happy_var_1
	)}

happyReduce_67 = happySpecReduce_3  15# happyReduction_67
happyReduction_67 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_1 of { (HappyWrap48 happy_var_1) -> 
	case happyOut49 happy_x_3 of { (HappyWrap49 happy_var_3) -> 
	happyIn49
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_68 = happySpecReduce_0  16# happyReduction_68
happyReduction_68  =  happyIn50
		 ([]
	)

happyReduce_69 = happySpecReduce_1  16# happyReduction_69
happyReduction_69 happy_x_1
	 =  case happyOut49 happy_x_1 of { (HappyWrap49 happy_var_1) -> 
	happyIn50
		 ((:[]) happy_var_1
	)}

happyReduce_70 = happySpecReduce_3  16# happyReduction_70
happyReduction_70 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut49 happy_x_1 of { (HappyWrap49 happy_var_1) -> 
	case happyOut50 happy_x_3 of { (HappyWrap50 happy_var_3) -> 
	happyIn50
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_71 = happyReduce 4# 17# happyReduction_71
happyReduction_71 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut38 happy_x_1 of { (HappyWrap38 happy_var_1) -> 
	case happyOut47 happy_x_2 of { (HappyWrap47 happy_var_2) -> 
	case happyOut45 happy_x_4 of { (HappyWrap45 happy_var_4) -> 
	happyIn51
		 (AbsHashedLang.ConstantDecl happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_72 = happySpecReduce_0  18# happyReduction_72
happyReduction_72  =  happyIn52
		 ([]
	)

happyReduce_73 = happySpecReduce_1  18# happyReduction_73
happyReduction_73 happy_x_1
	 =  case happyOut51 happy_x_1 of { (HappyWrap51 happy_var_1) -> 
	happyIn52
		 ((:[]) happy_var_1
	)}

happyReduce_74 = happySpecReduce_3  18# happyReduction_74
happyReduction_74 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut51 happy_x_1 of { (HappyWrap51 happy_var_1) -> 
	case happyOut52 happy_x_3 of { (HappyWrap52 happy_var_3) -> 
	happyIn52
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_75 = happySpecReduce_0  19# happyReduction_75
happyReduction_75  =  happyIn53
		 ([]
	)

happyReduce_76 = happySpecReduce_1  19# happyReduction_76
happyReduction_76 happy_x_1
	 =  case happyOut52 happy_x_1 of { (HappyWrap52 happy_var_1) -> 
	happyIn53
		 ((:[]) happy_var_1
	)}

happyReduce_77 = happySpecReduce_3  19# happyReduction_77
happyReduction_77 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut52 happy_x_1 of { (HappyWrap52 happy_var_1) -> 
	case happyOut53 happy_x_3 of { (HappyWrap53 happy_var_3) -> 
	happyIn53
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_78 = happySpecReduce_3  20# happyReduction_78
happyReduction_78 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut38 happy_x_1 of { (HappyWrap38 happy_var_1) -> 
	case happyOut64 happy_x_3 of { (HappyWrap64 happy_var_3) -> 
	happyIn54
		 (AbsHashedLang.LetDecl happy_var_1 happy_var_3
	)}}

happyReduce_79 = happySpecReduce_0  21# happyReduction_79
happyReduction_79  =  happyIn55
		 ([]
	)

happyReduce_80 = happySpecReduce_1  21# happyReduction_80
happyReduction_80 happy_x_1
	 =  case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	happyIn55
		 ((:[]) happy_var_1
	)}

happyReduce_81 = happySpecReduce_3  21# happyReduction_81
happyReduction_81 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	case happyOut55 happy_x_3 of { (HappyWrap55 happy_var_3) -> 
	happyIn55
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_82 = happySpecReduce_0  22# happyReduction_82
happyReduction_82  =  happyIn56
		 ([]
	)

happyReduce_83 = happySpecReduce_1  22# happyReduction_83
happyReduction_83 happy_x_1
	 =  case happyOut55 happy_x_1 of { (HappyWrap55 happy_var_1) -> 
	happyIn56
		 ((:[]) happy_var_1
	)}

happyReduce_84 = happySpecReduce_3  22# happyReduction_84
happyReduction_84 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut55 happy_x_1 of { (HappyWrap55 happy_var_1) -> 
	case happyOut56 happy_x_3 of { (HappyWrap56 happy_var_3) -> 
	happyIn56
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_85 = happySpecReduce_1  23# happyReduction_85
happyReduction_85 happy_x_1
	 =  case happyOut38 happy_x_1 of { (HappyWrap38 happy_var_1) -> 
	happyIn57
		 (AbsHashedLang.ConstantBound happy_var_1
	)}

happyReduce_86 = happySpecReduce_1  23# happyReduction_86
happyReduction_86 happy_x_1
	 =  case happyOut44 happy_x_1 of { (HappyWrap44 happy_var_1) -> 
	happyIn57
		 (AbsHashedLang.NumberBound happy_var_1
	)}

happyReduce_87 = happySpecReduce_3  24# happyReduction_87
happyReduction_87 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut64 happy_x_1 of { (HappyWrap64 happy_var_1) -> 
	case happyOut57 happy_x_3 of { (HappyWrap57 happy_var_3) -> 
	happyIn58
		 (AbsHashedLang.ConstraintLower happy_var_1 happy_var_3
	)}}

happyReduce_88 = happySpecReduce_3  24# happyReduction_88
happyReduction_88 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut64 happy_x_1 of { (HappyWrap64 happy_var_1) -> 
	case happyOut57 happy_x_3 of { (HappyWrap57 happy_var_3) -> 
	happyIn58
		 (AbsHashedLang.ConstraintUpper happy_var_1 happy_var_3
	)}}

happyReduce_89 = happySpecReduce_0  25# happyReduction_89
happyReduction_89  =  happyIn59
		 ([]
	)

happyReduce_90 = happySpecReduce_1  25# happyReduction_90
happyReduction_90 happy_x_1
	 =  case happyOut58 happy_x_1 of { (HappyWrap58 happy_var_1) -> 
	happyIn59
		 ((:[]) happy_var_1
	)}

happyReduce_91 = happySpecReduce_3  25# happyReduction_91
happyReduction_91 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut58 happy_x_1 of { (HappyWrap58 happy_var_1) -> 
	case happyOut59 happy_x_3 of { (HappyWrap59 happy_var_3) -> 
	happyIn59
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_92 = happySpecReduce_0  26# happyReduction_92
happyReduction_92  =  happyIn60
		 ([]
	)

happyReduce_93 = happySpecReduce_1  26# happyReduction_93
happyReduction_93 happy_x_1
	 =  case happyOut59 happy_x_1 of { (HappyWrap59 happy_var_1) -> 
	happyIn60
		 ((:[]) happy_var_1
	)}

happyReduce_94 = happySpecReduce_3  26# happyReduction_94
happyReduction_94 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut59 happy_x_1 of { (HappyWrap59 happy_var_1) -> 
	case happyOut60 happy_x_3 of { (HappyWrap60 happy_var_3) -> 
	happyIn60
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_95 = happySpecReduce_1  27# happyReduction_95
happyReduction_95 happy_x_1
	 =  case happyOut34 happy_x_1 of { (HappyWrap34 happy_var_1) -> 
	happyIn61
		 (AbsHashedLang.RA1D happy_var_1
	)}

happyReduce_96 = happyReduce 5# 27# happyReduction_96
happyReduction_96 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut34 happy_x_2 of { (HappyWrap34 happy_var_2) -> 
	case happyOut34 happy_x_4 of { (HappyWrap34 happy_var_4) -> 
	happyIn61
		 (AbsHashedLang.RA2D happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_97 = happyReduce 7# 27# happyReduction_97
happyReduction_97 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut34 happy_x_2 of { (HappyWrap34 happy_var_2) -> 
	case happyOut34 happy_x_4 of { (HappyWrap34 happy_var_4) -> 
	case happyOut34 happy_x_6 of { (HappyWrap34 happy_var_6) -> 
	happyIn61
		 (AbsHashedLang.RA3D happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_98 = happyReduce 5# 28# happyReduction_98
happyReduction_98 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut44 happy_x_3 of { (HappyWrap44 happy_var_3) -> 
	case happyOut64 happy_x_5 of { (HappyWrap64 happy_var_5) -> 
	happyIn62
		 (AbsHashedLang.PiecewiseCase happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_99 = happySpecReduce_3  28# happyReduction_99
happyReduction_99 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut64 happy_x_3 of { (HappyWrap64 happy_var_3) -> 
	happyIn62
		 (AbsHashedLang.PiecewiseFinalCase happy_var_3
	)}

happyReduce_100 = happySpecReduce_0  29# happyReduction_100
happyReduction_100  =  happyIn63
		 ([]
	)

happyReduce_101 = happySpecReduce_1  29# happyReduction_101
happyReduction_101 happy_x_1
	 =  case happyOut62 happy_x_1 of { (HappyWrap62 happy_var_1) -> 
	happyIn63
		 ((:[]) happy_var_1
	)}

happyReduce_102 = happySpecReduce_3  29# happyReduction_102
happyReduction_102 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut62 happy_x_1 of { (HappyWrap62 happy_var_1) -> 
	case happyOut63 happy_x_3 of { (HappyWrap63 happy_var_3) -> 
	happyIn63
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_103 = happySpecReduce_3  30# happyReduction_103
happyReduction_103 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut64 happy_x_1 of { (HappyWrap64 happy_var_1) -> 
	case happyOut65 happy_x_3 of { (HappyWrap65 happy_var_3) -> 
	happyIn64
		 (AbsHashedLang.EPlus happy_var_1 happy_var_3
	)}}

happyReduce_104 = happySpecReduce_3  30# happyReduction_104
happyReduction_104 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut64 happy_x_1 of { (HappyWrap64 happy_var_1) -> 
	case happyOut65 happy_x_3 of { (HappyWrap65 happy_var_3) -> 
	happyIn64
		 (AbsHashedLang.ERealImag happy_var_1 happy_var_3
	)}}

happyReduce_105 = happySpecReduce_3  30# happyReduction_105
happyReduction_105 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut64 happy_x_1 of { (HappyWrap64 happy_var_1) -> 
	case happyOut65 happy_x_3 of { (HappyWrap65 happy_var_3) -> 
	happyIn64
		 (AbsHashedLang.ESubtract happy_var_1 happy_var_3
	)}}

happyReduce_106 = happySpecReduce_1  30# happyReduction_106
happyReduction_106 happy_x_1
	 =  case happyOut65 happy_x_1 of { (HappyWrap65 happy_var_1) -> 
	happyIn64
		 (happy_var_1
	)}

happyReduce_107 = happyReduce 6# 30# happyReduction_107
happyReduction_107 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut64 happy_x_2 of { (HappyWrap64 happy_var_2) -> 
	case happyOut63 happy_x_5 of { (HappyWrap63 happy_var_5) -> 
	happyIn64
		 (AbsHashedLang.EPiecewise happy_var_2 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_108 = happySpecReduce_3  31# happyReduction_108
happyReduction_108 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut65 happy_x_1 of { (HappyWrap65 happy_var_1) -> 
	case happyOut66 happy_x_3 of { (HappyWrap66 happy_var_3) -> 
	happyIn65
		 (AbsHashedLang.EMul happy_var_1 happy_var_3
	)}}

happyReduce_109 = happySpecReduce_3  31# happyReduction_109
happyReduction_109 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut65 happy_x_1 of { (HappyWrap65 happy_var_1) -> 
	case happyOut66 happy_x_3 of { (HappyWrap66 happy_var_3) -> 
	happyIn65
		 (AbsHashedLang.EDiv happy_var_1 happy_var_3
	)}}

happyReduce_110 = happySpecReduce_1  31# happyReduction_110
happyReduction_110 happy_x_1
	 =  case happyOut66 happy_x_1 of { (HappyWrap66 happy_var_1) -> 
	happyIn65
		 (happy_var_1
	)}

happyReduce_111 = happySpecReduce_3  32# happyReduction_111
happyReduction_111 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut66 happy_x_1 of { (HappyWrap66 happy_var_1) -> 
	case happyOut67 happy_x_3 of { (HappyWrap67 happy_var_3) -> 
	happyIn66
		 (AbsHashedLang.EScale happy_var_1 happy_var_3
	)}}

happyReduce_112 = happySpecReduce_3  32# happyReduction_112
happyReduction_112 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut66 happy_x_1 of { (HappyWrap66 happy_var_1) -> 
	case happyOut67 happy_x_3 of { (HappyWrap67 happy_var_3) -> 
	happyIn66
		 (AbsHashedLang.EDot happy_var_1 happy_var_3
	)}}

happyReduce_113 = happySpecReduce_1  32# happyReduction_113
happyReduction_113 happy_x_1
	 =  case happyOut67 happy_x_1 of { (HappyWrap67 happy_var_1) -> 
	happyIn66
		 (happy_var_1
	)}

happyReduce_114 = happySpecReduce_3  33# happyReduction_114
happyReduction_114 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut67 happy_x_1 of { (HappyWrap67 happy_var_1) -> 
	case happyOut34 happy_x_3 of { (HappyWrap34 happy_var_3) -> 
	happyIn67
		 (AbsHashedLang.EPower happy_var_1 happy_var_3
	)}}

happyReduce_115 = happySpecReduce_1  33# happyReduction_115
happyReduction_115 happy_x_1
	 =  case happyOut68 happy_x_1 of { (HappyWrap68 happy_var_1) -> 
	happyIn67
		 (happy_var_1
	)}

happyReduce_116 = happySpecReduce_2  34# happyReduction_116
happyReduction_116 happy_x_2
	happy_x_1
	 =  case happyOut38 happy_x_1 of { (HappyWrap38 happy_var_1) -> 
	case happyOut69 happy_x_2 of { (HappyWrap69 happy_var_2) -> 
	happyIn68
		 (AbsHashedLang.EFun happy_var_1 happy_var_2
	)}}

happyReduce_117 = happySpecReduce_3  34# happyReduction_117
happyReduction_117 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut61 happy_x_2 of { (HappyWrap61 happy_var_2) -> 
	case happyOut69 happy_x_3 of { (HappyWrap69 happy_var_3) -> 
	happyIn68
		 (AbsHashedLang.ERotate happy_var_2 happy_var_3
	)}}

happyReduce_118 = happySpecReduce_1  34# happyReduction_118
happyReduction_118 happy_x_1
	 =  case happyOut69 happy_x_1 of { (HappyWrap69 happy_var_1) -> 
	happyIn68
		 (happy_var_1
	)}

happyReduce_119 = happySpecReduce_3  35# happyReduction_119
happyReduction_119 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut64 happy_x_2 of { (HappyWrap64 happy_var_2) -> 
	happyIn69
		 (happy_var_2
	)}

happyReduce_120 = happySpecReduce_1  35# happyReduction_120
happyReduction_120 happy_x_1
	 =  case happyOut44 happy_x_1 of { (HappyWrap44 happy_var_1) -> 
	happyIn69
		 (AbsHashedLang.ENum happy_var_1
	)}

happyReduce_121 = happySpecReduce_1  35# happyReduction_121
happyReduction_121 happy_x_1
	 =  case happyOut38 happy_x_1 of { (HappyWrap38 happy_var_1) -> 
	happyIn69
		 (AbsHashedLang.EIdent happy_var_1
	)}

happyNewToken action sts stk [] =
	happyDoAction 43# notHappyAtAll action sts stk []

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
	PT _ (TS _ 29) -> cont 29#;
	PT _ (TS _ 30) -> cont 30#;
	PT _ (TS _ 31) -> cont 31#;
	PT _ (TS _ 32) -> cont 32#;
	PT _ (TS _ 33) -> cont 33#;
	PT _ (TS _ 34) -> cont 34#;
	PT _ (TS _ 35) -> cont 35#;
	PT _ (TS _ 36) -> cont 36#;
	PT _ (TS _ 37) -> cont 37#;
	PT _ (TI happy_dollar_dollar) -> cont 38#;
	PT _ (TD happy_dollar_dollar) -> cont 39#;
	PT _ (TL happy_dollar_dollar) -> cont 40#;
	PT _ (T_KWDataPattern happy_dollar_dollar) -> cont 41#;
	PT _ (T_PIdent _) -> cont 42#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 43# tk tks = happyError' (tks, explist)
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
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {(HappyWrap39 x') = happyOut39 x} in x'))

pBlock tks = happySomeParser where
 happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (let {(HappyWrap40 x') = happyOut40 x} in x'))

pListBlock tks = happySomeParser where
 happySomeParser = happyThen (happyParse 2# tks) (\x -> happyReturn (let {(HappyWrap41 x') = happyOut41 x} in x'))

pZ tks = happySomeParser where
 happySomeParser = happyThen (happyParse 3# tks) (\x -> happyReturn (let {(HappyWrap42 x') = happyOut42 x} in x'))

pR tks = happySomeParser where
 happySomeParser = happyThen (happyParse 4# tks) (\x -> happyReturn (let {(HappyWrap43 x') = happyOut43 x} in x'))

pNumber tks = happySomeParser where
 happySomeParser = happyThen (happyParse 5# tks) (\x -> happyReturn (let {(HappyWrap44 x') = happyOut44 x} in x'))

pVal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 6# tks) (\x -> happyReturn (let {(HappyWrap45 x') = happyOut45 x} in x'))

pDim tks = happySomeParser where
 happySomeParser = happyThen (happyParse 7# tks) (\x -> happyReturn (let {(HappyWrap46 x') = happyOut46 x} in x'))

pShape tks = happySomeParser where
 happySomeParser = happyThen (happyParse 8# tks) (\x -> happyReturn (let {(HappyWrap47 x') = happyOut47 x} in x'))

pVariableDecl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 9# tks) (\x -> happyReturn (let {(HappyWrap48 x') = happyOut48 x} in x'))

pListVariableDecl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 10# tks) (\x -> happyReturn (let {(HappyWrap49 x') = happyOut49 x} in x'))

pListListVariableDecl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 11# tks) (\x -> happyReturn (let {(HappyWrap50 x') = happyOut50 x} in x'))

pConstantDecl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 12# tks) (\x -> happyReturn (let {(HappyWrap51 x') = happyOut51 x} in x'))

pListConstantDecl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 13# tks) (\x -> happyReturn (let {(HappyWrap52 x') = happyOut52 x} in x'))

pListListConstantDecl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 14# tks) (\x -> happyReturn (let {(HappyWrap53 x') = happyOut53 x} in x'))

pLetDecl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 15# tks) (\x -> happyReturn (let {(HappyWrap54 x') = happyOut54 x} in x'))

pListLetDecl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 16# tks) (\x -> happyReturn (let {(HappyWrap55 x') = happyOut55 x} in x'))

pListListLetDecl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 17# tks) (\x -> happyReturn (let {(HappyWrap56 x') = happyOut56 x} in x'))

pBound tks = happySomeParser where
 happySomeParser = happyThen (happyParse 18# tks) (\x -> happyReturn (let {(HappyWrap57 x') = happyOut57 x} in x'))

pConstraintDecl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 19# tks) (\x -> happyReturn (let {(HappyWrap58 x') = happyOut58 x} in x'))

pListConstraintDecl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 20# tks) (\x -> happyReturn (let {(HappyWrap59 x') = happyOut59 x} in x'))

pListListConstraintDecl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 21# tks) (\x -> happyReturn (let {(HappyWrap60 x') = happyOut60 x} in x'))

pRotateAmount tks = happySomeParser where
 happySomeParser = happyThen (happyParse 22# tks) (\x -> happyReturn (let {(HappyWrap61 x') = happyOut61 x} in x'))

pPiecewiseCase tks = happySomeParser where
 happySomeParser = happyThen (happyParse 23# tks) (\x -> happyReturn (let {(HappyWrap62 x') = happyOut62 x} in x'))

pListPiecewiseCase tks = happySomeParser where
 happySomeParser = happyThen (happyParse 24# tks) (\x -> happyReturn (let {(HappyWrap63 x') = happyOut63 x} in x'))

pExp tks = happySomeParser where
 happySomeParser = happyThen (happyParse 25# tks) (\x -> happyReturn (let {(HappyWrap64 x') = happyOut64 x} in x'))

pExp1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 26# tks) (\x -> happyReturn (let {(HappyWrap65 x') = happyOut65 x} in x'))

pExp2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 27# tks) (\x -> happyReturn (let {(HappyWrap66 x') = happyOut66 x} in x'))

pExp3 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 28# tks) (\x -> happyReturn (let {(HappyWrap67 x') = happyOut67 x} in x'))

pExp4 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 29# tks) (\x -> happyReturn (let {(HappyWrap68 x') = happyOut68 x} in x'))

pExp5 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 30# tks) (\x -> happyReturn (let {(HappyWrap69 x') = happyOut69 x} in x'))

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
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $













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



















data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
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
         off_i  = (off Happy_GHC_Exts.+# i)
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
             off_i = (off Happy_GHC_Exts.+# nt)
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
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ((Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

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
