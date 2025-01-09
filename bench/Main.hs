{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.DeepSeq
    ( rnf )
import Control.Exception
    ( evaluate )
import Test.Tasty.Bench
    ( bench, bgroup, defaultMain, nf )

import qualified Data.Set as Set
import Data.Monoid.Monus (Monus(symmetricDifference))
import Prelude hiding ((^))
import qualified Prelude
import Numeric.Natural (Natural)
import Data.Set (Set)
import Data.Semigroup (Product(Product))

main :: IO ()
main = do

    evaluate $ rnf [setA, setB]
    evaluate $ rnf [natA, natB]

    defaultMain
        [ bgroup "Monus"
            [ bgroup "symmetricDifference"
                [ bench "Set" $
                    nf (symmetricDifference setA) setB
                , bench "Product Natural" $
                    nf (symmetricDifference (Product natA)) (Product natB)
                ]
            ]
        ]
  where
    setA :: Set Natural
    setA = Set.fromList $ filter (\n -> (n `mod` 3) /= 0) $ [1 .. 2 ^ 16]

    setB :: Set Natural
    setB = Set.fromList $ filter (\n -> (n `mod` 3) /= 1) $ [1 .. 2 ^ 16]

    natA :: Natural
    natA = (2 ^ 32768) - 1

    natB :: Natural
    natB = (2 ^ 65536) - 1

(^) :: Integral i => i -> i -> i
(^) = (Prelude.^)


