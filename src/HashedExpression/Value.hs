module HashedExpression.Value where

import Data.Array
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import HashedExpression.Internal.Expression

type Dataset = String

data DataFile
  = TXT FilePath
  | -- C support hdf5, we want npy too but haven't found any npy reader library for C
    -- file path to the data file from your solver
    HDF5 FilePath Dataset -- HDF5 requires the name of the data set in the data file
  deriving (Eq, Show, Ord)

data Val
  = VScalar Double
  | V1D (Array Int Double)
  | V2D (Array (Int, Int) Double)
  | V3D (Array (Int, Int, Int) Double)
  | VFile DataFile
  | VNum Double
  deriving (Eq, Show, Ord)

type ValMaps = Map String Val

-- |
valElems :: Val -> [Double]
valElems val =
  case val of
    VScalar v -> [v]
    V1D vs -> elems vs
    V2D vs -> elems vs
    V3D vs -> elems vs
    _ -> []

valueFromHaskell :: Val -> Bool
valueFromHaskell val =
  case val of
    VScalar v -> True
    V1D vs -> True
    V2D vs -> True
    V3D vs -> True
    _ -> False

-- |
compatible :: Shape -> Val -> Bool
compatible shape v =
  case (shape, v) of
    (_, VNum _) -> True
    (_, VFile _) -> True
    ([], VScalar val) -> True
    ([x], V1D arr1d)
      | bounds arr1d == (0, x - 1) -> True
    ([x, y], V2D arr2d)
      | bounds arr2d == ((0, 0), (x - 1, y - 1)) -> True
    ([x, y, z], V3D arr3d)
      | bounds arr3d == ((0, 0, 0), (x - 1, y - 1, z - 1)) -> True
    _ -> False
