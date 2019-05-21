Specify the packing of arrays in memory.
\begin{code}
module HashedPacking where

--import HashedExpression
--import HashedInstances

import Data.ByteString (ByteString)
--import qualified Data.ByteString.Char8 as C
\end{code}

\begin{code}
data PackAlignment = PA1 | PA2 | PA4 | PA8 | PA16 | PA32 | PA64 | PA128 | PA256
  deriving (Show,Eq,Ord)
data PackTypes = PackFloat | PackDouble
\end{code}
\begin{code}
data PackOne = Pack  [(Int,Int)]     -- dimensions
                     [Int]           -- order of nesting CKA:  not yet implementing
                     PackTypes       -- binary storage type
                     PackAlignment   -- alignment of elements
\end{code}
List of layout of variables in memory.  Assume we get one pointer to all variables.  
This is a temporary scheme which will need to be revised to take multiple cores into account.
\begin{code}
type Packing = [(ByteString,PackOne)]
\end{code}
\begin{code}
\end{code}
\begin{code}
\end{code}
