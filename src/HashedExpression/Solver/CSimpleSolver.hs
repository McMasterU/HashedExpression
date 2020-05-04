module HashedExpression.Solver.CSimpleSolver where

import Control.Monad (when)
import Data.List (find, sortOn)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import HashedExpression.Codegen
import HashedExpression.Codegen.CSimple
import HashedExpression.Embed.FFTW (fftUtils)
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Inner
import HashedExpression.Internal.Node
import HashedExpression.Internal.Normalize (normalize)
import HashedExpression.Internal.Utils
import HashedExpression.Problem

