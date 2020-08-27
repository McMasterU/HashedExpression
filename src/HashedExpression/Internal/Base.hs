-- |
-- Module      :  HashedExpression.Internal.Base
-- Copyright   :  (c) OCA 2020
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  anandc@mcmaster.ca
-- Stability   :  provisional
-- Portability :  unportable
--
-- This modules contains types that should be imported only by Internal modules
module HashedExpression.Internal.Base where

newtype NodeID = NodeID {unNodeID :: Int} deriving (Eq, Ord)

instance Show NodeID where
  show (NodeID nID) = show nID
