module Main where

import Data.Array
import qualified Data.IntMap.Strict as IM
import Data.Map (fromList, union)
import qualified Data.Set as Set
import HashedDerivative
import HashedExpression
import HashedInterp
import HashedOperation hiding (product, sum)
import qualified HashedOperation
import HashedPrettify
import HashedSimplify
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

import Control.DeepSeq (deepseq)
import Data.Complex (Complex(..))
import Data.List (intercalate)
import Data.Maybe (fromJust)
import HashedToC (generateProgram)
import HashedUtils
import HashedVar
import Test.Hspec
import Test.QuickCheck hiding (scale)

main = do
    let sum = fromJust . HashedOperation.sum
    let valMaps =
            ValMaps
                { vm0 =
                      fromList
                          [ ("a", 1)
                          , ("d", 1)
                          , ("e", 1)
                          , ("p", 1)
                          , ("w", 1)
                          , ("z", 2)
                          ]
                , vm1 = fromList []
                , vm2 = fromList []
                , vm3 = fromList []
                }
    putStrLn "---------KIKI--------"
    let kiki =
            sum
                [ (const 0 +: ((z * a) + (a * z)))
                , (((z * d) + ((a * w))) +: ((z * w) + (a * d)))
                ]
--    showAllEntries kiki
    let valSimp = "" `deepseq` simplify kiki
    showExp kiki
    showExp $ valSimp
    print $ eval valMaps kiki
    print $ eval valMaps $ valSimp
    let (mp, n) =
            ( fromList
                  [ ( -5818692093993289625
                    , ([], Sum C [4413262092026763468, 4555606689700716517]))
                  , (97, ([], Var "a"))
                  , (100, ([], Var "d"))
                  , (119, ([], Var "w"))
                  , (122, ([], Var "z"))
                  , (237419851, ([], Mul R [122, 97]))
                  , (242557047, ([], Mul R [97, 100]))
                  , (244445722, ([], Mul R [122, 100]))
                  , (287054230, ([], Mul R [97, 119]))
                  , (288942905, ([], Mul R [122, 119]))
                  , (294080101, ([], Mul R [97, 122]))
                  , (294306742, ([], Mul R [100, 122]))
                  , (295742135, ([], Mul R [119, 122]))
                  , (79088992115, ([], Const 0.0))
                  , (515818251688626, ([], Sum R [288942905, 242557047]))
                  , (604004103873516, ([], Sum R [244445722, 287054230]))
                  , (617928185797446, ([], Sum R [237419851, 294080101]))
                  , (621671218572696, ([], Sum R [294080101, 294080101]))
                  , ( 1608304904582259263
                    , ([], Sum C [4386690302355263718, 3800110708524591367]))
                  , ( 3800110708524591367
                    , ([], RealImag 604004103873516 515818251688626))
                  , ( 4386690302355263718
                    , ([], RealImag 79088992115 617928185797446))
                  , ( 4413262092026763468
                    , ([], RealImag 79088992115 621671218572696))
                  , ( 4555606689700716517
                    , ([], RealImag 621671218572696 621671218572696))
                  ]
            , -5818692093993289625)
    print "hello world"
    showAllEntries $ (a*w)+(d*z)
    showAllEntries $ (a*d)+(w*z)
--    putStrLn "_------+_______________________-"
--    let hihi = (a * z) + (w * z)
--    showExp hihi
--    showExp $ simplify hihi
