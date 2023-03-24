{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}

-- | This module defines the 'LCMMonoid' subclass of the 'Monoid' class.
--
-- The 'LCMMonoid' subclass adds the 'lcm' operation, which takes two monoidal
-- arguments and finds their /least common multiple/, or (more generally) the
-- least monoid from which either argument can be subtracted with the '</>'
-- operation.
--
-- The 'LCMMonoid' class is for Abelian, /i.e./, 'Commutative' monoids.
--
module Data.Monoid.LCM (
    LCMMonoid (..)
    )
where

import Prelude hiding (gcd, lcm, max)
import qualified Prelude

import Data.IntSet (IntSet)
import Data.Maybe (isJust)
import Data.Monoid (Dual (..), Product (..), Sum (..))
import Data.Monoid.GCD (GCDMonoid (..))
import Data.Semigroup.Cancellative (Reductive ((</>)))
import Data.Semigroup.Commutative (Commutative)
import Data.Set (Set)
import Numeric.Natural (Natural)
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set

-- | Class of Abelian monoids that allow the /least common multiple/ to be
--   found for any two given values.
--
-- Operations must satisfy the following laws:
--
-- __/Reductivity/__
--
-- @
-- 'isJust' ('lcm' a b '</>' a)
-- @
-- @
-- 'isJust' ('lcm' a b '</>' b)
-- @
--
-- __/Uniqueness/__
--
-- @
-- 'all' 'isJust'
--     [ \   \   c '</>' a
--     , \   \   c '</>' b
--     , 'lcm' a b '</>' c
--     ]
-- ==>
--     ('lcm' a b '==' c)
-- @
--
-- __/Idempotence/__
--
-- @
-- 'lcm' a a '==' a
-- @
--
-- __/Identity/__
--
-- @
-- 'lcm' 'mempty' a '==' a
-- @
-- @
-- 'lcm' a 'mempty' '==' a
-- @
--
-- __/Commutativity/__
--
-- @
-- 'lcm' a b '==' 'lcm' b a
-- @
--
-- __/Associativity/__
--
-- @
-- 'lcm' ('lcm' a b) c '==' 'lcm' a ('lcm' b c)
-- @
--
-- __/Absorption/__
--
-- @
-- 'lcm' a ('gcd' a b) '==' a
-- @
-- @
-- 'gcd' a ('lcm' a b) '==' a
-- @
--
class GCDMonoid m => LCMMonoid m where
    lcm :: m -> m -> m

instance LCMMonoid () where
    lcm () () = ()

instance LCMMonoid a => LCMMonoid (Dual a) where
    lcm (Dual a) (Dual b) = Dual (lcm a b)

instance LCMMonoid (Product Natural) where
    lcm (Product a) (Product b) = Product (Prelude.lcm a b)

instance LCMMonoid (Sum Natural) where
    lcm (Sum a) (Sum b) = Sum (Prelude.max a b)

instance Ord a => LCMMonoid (Set a) where
    lcm = Set.union

instance LCMMonoid IntSet where
    lcm = IntSet.union

instance (LCMMonoid a, LCMMonoid b) => LCMMonoid (a, b) where
    lcm (a0, a1) (b0, b1) =
        (lcm a0 b0, lcm a1 b1)

instance (LCMMonoid a, LCMMonoid b, LCMMonoid c) => LCMMonoid (a, b, c) where
    lcm (a0, a1, a2) (b0, b1, b2) =
        (lcm a0 b0, lcm a1 b1, lcm a2 b2)

instance (LCMMonoid a, LCMMonoid b, LCMMonoid c, LCMMonoid d) =>
    LCMMonoid (a, b, c, d)
  where
    lcm (a0, a1, a2, a3) (b0, b1, b2, b3) =
        (lcm a0 b0, lcm a1 b1, lcm a2 b2, lcm a3 b3)

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
    _isJust :: Maybe m -> Bool
    _isJust = isJust
    _reduce :: Reductive m => m -> m -> Maybe m
    _reduce = (</>)
    _commutative :: Commutative m => ()
    _commutative = ()
