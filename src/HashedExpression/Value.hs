-- |
-- Module      :  HashedExpression.Value
-- Copyright   :  (c) OCA 2020
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  anandc@mcmaster.ca
-- Stability   :  provisional
-- Portability :  unportable
--
-- This module contains functionality for defining values to be subbed into a 'TypedExpr'. An 'TypedExpr' is provided values via a 'ValMap',
-- which should be constructed like any other 'Map', for example
--
-- @
--  valMap = fromList [("x",v1),("y",v2),....]
-- @
-- where @"x","y"@ are identifiers and @v1,v2@ are of type 'Val'
module HashedExpression.Value where

import Data.Array
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import HashedExpression.Internal.Base

-- | The name of a dataset within an HDF5 file
type Dataset = String

-- | The types of data files supported by HashedExpression
data DataFile
  = -- | File path to plaintext data file from your solver
    TXT FilePath
  | -- | HDF5 requires the name of the data set in the data file,
    --   C supports hdf5, we want npy too but haven't found any npy reader library for C
    HDF5 FilePath Dataset
  deriving (Eq, Show, Ord)

-- | A value
data Val
  = -- | Constant scalar double value
    VScalar Double
  | -- | One-dimensional double array
    V1D (Array Int Double)
  | -- | Two-dimensional double array
    V2D (Array (Int, Int) Double)
  | -- | Three-dimensional double array
    V3D (Array (Int, Int, Int) Double)
  | -- | Path to a data file
    VFile DataFile
  | -- | A constant value for any shapes (e.g. a 100-by-200 matrix with every entry equal to 2.13)
    VNum Double
  deriving (Eq, Show, Ord)

-- | A mapping from variable name to value
type ValMap = Map String Val

-- | Extract the value elements from the value
valElems :: Val -> [Double]
valElems val =
  case val of
    VScalar v -> [v]
    V1D vs -> elems vs
    V2D vs -> elems vs
    V3D vs -> elems vs
    _ -> []

-- | Returns true if the value is given from within Haskell,
--   false if it is only known at runtime of the C code
valueFromHaskell :: Val -> Bool
valueFromHaskell val =
  case val of
    VScalar _ -> True
    V1D vs -> True
    V2D vs -> True
    V3D vs -> True
    _ -> False

-- | Returns true if a shape and a value is compatible with one another, false otherwise
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
