module HashedSimplify where

import HashedExpression
import HashedOperation


simplify :: (DimensionType d, ElementType et) => Expression d et -> Expression d et
simplify = id

