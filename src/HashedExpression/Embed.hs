module HashedExpression.Embed where

import Data.FileEmbed

-- | Utilities for computing FFT
fftUtils :: String
fftUtils = $(embedStringFile "embed/fftw.c")

-- | Version of HashedExpression
version :: String
version = $(embedStringFile "VERSION")
