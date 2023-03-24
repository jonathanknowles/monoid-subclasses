{-# LANGUAGE FlexibleInstances #-}

-- | This module defines subclasses of GCD monoids with /distributivity/:
--
-- - 'DistributiveGCDMonoid'
--
--     Subclass of 'GCDMonoid' with /symmetric/ distributivity.
--
-- - 'LeftDistributiveGCDMonoid'
--
--     Subclass of 'LeftGCDMonoid' with /left/-distributivity.
--
-- - 'RightDistributiveGCDMonoid'
--
--     Subclass of 'RightGCDMonoid' with /right/-distributivity.
--
module Data.Monoid.GCD.Distributive
    ( DistributiveGCDMonoid
    , LeftDistributiveGCDMonoid
    , RightDistributiveGCDMonoid
    )
    where

import Data.Monoid
    ( Dual, Product, Sum )
import Data.IntSet
    ( IntSet )
import Data.Monoid.GCD
    ( GCDMonoid (gcd)
    , LeftGCDMonoid (commonPrefix)
    , RightGCDMonoid (commonSuffix)
    )
import Data.Sequence
    ( Seq )
import Data.Set
    ( Set )
import Numeric.Natural
    ( Natural )
import Prelude hiding
    ( gcd )

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy

--------------------------------------------------------------------------------
-- DistributiveGCDMonoid
--------------------------------------------------------------------------------

-- | Class of /commutative/ GCD monoids with /symmetric/ distributivity.
--
-- In addition to the general 'GCDMonoid' laws, instances of this class
-- must also satisfy the following laws:
--
-- @
-- 'gcd' (a '<>' b) (a '<>' c) '==' a '<>' 'gcd' b c
-- @
-- @
-- 'gcd' (a '<>' c) (b '<>' c) '==' 'gcd' a b '<>' c
-- @
--
class (LeftDistributiveGCDMonoid m, RightDistributiveGCDMonoid m, GCDMonoid m)
    => DistributiveGCDMonoid m

instance DistributiveGCDMonoid ()
instance DistributiveGCDMonoid (Product Natural)
instance DistributiveGCDMonoid (Sum Natural)
instance DistributiveGCDMonoid IntSet
instance DistributiveGCDMonoid a => DistributiveGCDMonoid (Dual a)
instance Ord a => DistributiveGCDMonoid (Set a)

--------------------------------------------------------------------------------
-- LeftDistributiveGCDMonoid
--------------------------------------------------------------------------------

-- | Class of /left/ GCD monoids with /left/-distributivity.
--
-- In addition to the general 'LeftGCDMonoid' laws, instances of this class
-- must also satisfy the following law:
--
-- @
-- 'commonPrefix' (a '<>' b) (a '<>' c) '==' a '<>' 'commonPrefix' b c
-- @
--
class LeftGCDMonoid m => LeftDistributiveGCDMonoid m

-- Instances for non-commutative monoids:
instance Eq a => LeftDistributiveGCDMonoid [a]
instance Eq a => LeftDistributiveGCDMonoid (Seq a)
instance LeftDistributiveGCDMonoid ByteString.ByteString
instance LeftDistributiveGCDMonoid ByteString.Lazy.ByteString
instance LeftDistributiveGCDMonoid Text.Text
instance LeftDistributiveGCDMonoid Text.Lazy.Text

-- Instances for commutative monoids:
instance LeftDistributiveGCDMonoid ()
instance LeftDistributiveGCDMonoid (Product Natural)
instance LeftDistributiveGCDMonoid (Sum Natural)
instance LeftDistributiveGCDMonoid IntSet
instance Ord a => LeftDistributiveGCDMonoid (Set a)

-- Instances for monoid transformers:
instance RightDistributiveGCDMonoid a => LeftDistributiveGCDMonoid (Dual a)

--------------------------------------------------------------------------------
-- RightDistributiveGCDMonoid
--------------------------------------------------------------------------------

-- | Class of /right/ GCD monoids with /right/-distributivity.
--
-- In addition to the general 'RightGCDMonoid' laws, instances of this class
-- must also satisfy the following law:
--
-- @
-- 'commonSuffix' (a '<>' c) (b '<>' c) '==' 'commonSuffix' a b '<>' c
-- @
--
class RightGCDMonoid m => RightDistributiveGCDMonoid m

-- Instances for non-commutative monoids:
instance Eq a => RightDistributiveGCDMonoid [a]
instance Eq a => RightDistributiveGCDMonoid (Seq a)
instance RightDistributiveGCDMonoid ByteString.ByteString
instance RightDistributiveGCDMonoid ByteString.Lazy.ByteString
instance RightDistributiveGCDMonoid Text.Text
instance RightDistributiveGCDMonoid Text.Lazy.Text

-- Instances for commutative monoids:
instance RightDistributiveGCDMonoid ()
instance RightDistributiveGCDMonoid (Product Natural)
instance RightDistributiveGCDMonoid (Sum Natural)
instance RightDistributiveGCDMonoid IntSet
instance Ord a => RightDistributiveGCDMonoid (Set a)

-- Instances for monoid transformers:
instance LeftDistributiveGCDMonoid a => RightDistributiveGCDMonoid (Dual a)

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
    _commonPrefix :: LeftGCDMonoid m => m -> m -> m
    _commonPrefix = commonPrefix
    _commonSuffix :: RightGCDMonoid m => m -> m -> m
    _commonSuffix = commonSuffix
