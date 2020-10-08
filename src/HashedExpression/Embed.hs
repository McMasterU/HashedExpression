module HashedExpression.Embed where

import Data.FileEmbed
import Data.List (foldl')
import qualified Data.Text as T

-- | Utilities for computing FFT
fftUtils :: String
fftUtils = $(embedStringFile "embed/fftw.c")

-- | Version of HashedExpression
version :: String
version = $(embedStringFile "VERSION")

-- | CSimple template
cSimpleTemplate :: T.Text
cSimpleTemplate = $(embedStringFile "embed/csimple.c")

randomizeValueTemplate :: T.Text
randomizeValueTemplate = $(embedStringFile "embed/random.c")

readTXTTemplate :: T.Text
readTXTTemplate = $(embedStringFile "embed/readTXT.c")

readHDF5Template :: T.Text
readHDF5Template = $(embedStringFile "embed/readHDF5.c")

writeTXTTemplate :: T.Text
writeTXTTemplate = $(embedStringFile "embed/writeTXT.c")

writeHDF5Template :: T.Text
writeHDF5Template = $(embedStringFile "embed/writeHDF5.c")

renderTemplate :: [(T.Text, T.Text)] -> T.Text -> T.Text
renderTemplate replacement template = foldl' f template replacement
  where
    f acc (name, val) = T.replace ("%{" <> name <> "}") val acc