{-
Specify the packing of arrays in memory.
-}
module HashedPacking where

--import HashedExpression
--import HashedInstances
import Data.ByteString (ByteString)

--import qualified Data.ByteString.Char8 as C
{-

-}
data PackAlignment
    = PA1
    | PA2
    | PA4
    | PA8
    | PA16
    | PA32
    | PA64
    | PA128
    | PA256
    deriving (Show, Eq, Ord)

data PackTypes
    = PackFloat
    | PackDouble

{-
-}
data PackOne =
    Pack
        [(Int, Int)] -- dimensions
        [Int] -- order of nesting CKA:  not yet implementing
        PackTypes -- binary storage type
        PackAlignment -- alignment of elements

{-
List of layout of variables in memory.  Assume we get one pointer to all variables.
This is a temporary scheme which will need to be revised to take multiple cores into account.
-}
type Packing = [(ByteString, PackOne)] {-
-}
{-
-}
{-
-}
