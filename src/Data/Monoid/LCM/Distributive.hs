{-# LANGUAGE FlexibleInstances #-}

-- | This module defines the 'DistributiveLCMMonoid' subclass of 'LCMMonoid',
--   for /commutative/ LCM monoids with /distributivity/.
--
module Data.Monoid.LCM.Distributive
    ( DistributiveLCMMonoid
    )
    where

import Data.IntSet
    ( IntSet )
import Data.Monoid
    ( Dual, Product, Sum )
import Data.Monoid.GCD
    ( GCDMonoid (gcd) )
import Data.Monoid.GCD.Distributive
    ( DistributiveGCDMonoid )
import Data.Monoid.LCM
    ( LCMMonoid (lcm) )
import Data.Set
    ( Set )
import Numeric.Natural
    ( Natural )
import Prelude hiding
    ( gcd, lcm )

-- | Class of /commutative/ LCM monoids with /distributivity/.
--
-- In addition to the general 'LCMMonoid' laws, instances of this class
-- must also satisfy the following laws:
--
-- The 'lcm' operation itself must be /both/ left-distributive /and/
-- right-distributive:
--
-- @
-- 'lcm' (a '<>' b) (a '<>' c) '==' a '<>' 'lcm' b c
-- @
-- @
-- 'lcm' (a '<>' c) (b '<>' c) '==' 'lcm' a b '<>' c
-- @
--
-- The 'lcm' and 'gcd' operations must distribute over one another:
--
-- @
-- 'lcm' a ('gcd' b c) '==' 'gcd' ('lcm' a b) ('lcm' a c)
-- @
-- @
-- 'gcd' a ('lcm' b c) '==' 'lcm' ('gcd' a b) ('gcd' a c)
-- @
--
class (DistributiveGCDMonoid m, LCMMonoid m) => DistributiveLCMMonoid m

instance DistributiveLCMMonoid ()
instance DistributiveLCMMonoid (Product Natural)
instance DistributiveLCMMonoid (Sum Natural)
instance DistributiveLCMMonoid IntSet
instance DistributiveLCMMonoid a => DistributiveLCMMonoid (Dual a)
instance Ord a => DistributiveLCMMonoid (Set a)

--------------------------------------------------------------------------------
-- Internal
--------------------------------------------------------------------------------

-- The following definitions must be imported in order for Haddock to be able
-- to construct hyperlinks within documentation, but are not actually used in
-- any function. We reference them here to stop GHC from marking them as
-- redundant imports:
--
_nonRedundantImports :: ()
_nonRedundantImports = ()
  where
    _gcd :: GCDMonoid m => m -> m -> m
    _gcd = gcd
    _lcm :: LCMMonoid m => m -> m -> m
    _lcm = lcm
