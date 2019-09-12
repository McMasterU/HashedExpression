{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FunctionalDependencies #-}

module Nothing where

import Data.Proxy
import GHC.TypeLits

data Zero
    deriving (DimensionType)

data One
    deriving (DimensionType)

data Two
    deriving (DimensionType)

data Three
    deriving (DimensionType)

class DimensionType a

instance (KnownNat a) => DimensionType a

instance (KnownNat a, KnownNat b) => DimensionType '(a, b)

expr :: Proxy a
expr = Proxy

class Op a b c | a b -> c where
    (<.>) :: a -> b -> c

instance DimensionType d => Op (Proxy d) (Proxy d) (Proxy Zero) where
    (<.>) x y = Proxy

main :: IO ()
main = do
    let a = expr :: Proxy '(10, 10)
    let b = expr :: Proxy '(10, 10)
    let c = a <.> b
    print "Hello World"
    print "Hello World"

