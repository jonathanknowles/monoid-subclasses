{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.DeepSeq
    ( rnf )
import Control.Exception
    ( evaluate )
import Test.Tasty.Bench
    ( bench, bgroup, defaultMain, nf )

import qualified Data.Set as Set
import Data.Monoid.Monus (Monus(symmetricDifference))
import Prelude
import Data.Monoid.Monus (symmetricDifferenceSetMerge)
import Data.Monoid.Monus (symmetricDifferenceSetDefault)

main :: IO ()
main = do

    evaluate $ rnf [setA, setB]

    putStrLn $
        show $
        symmetricDifferenceSetMerge   setA setB ==
        symmetricDifferenceSetDefault setA setB

    defaultMain
        [ bgroup "Monus"
            [ bgroup "symmetricDifference"
                [ bench "Set" $
                    nf (symmetricDifference setA) setB
                ]
            ]
        ]
  where
    bound :: Int
    bound = 2 ^ (16 :: Int)

    naturals :: [Int]
    naturals = [1 .. bound]

    setA = Set.fromList $ filter (\n -> (n `mod` 3) /= 0) naturals
    setB = Set.fromList $ filter (\n -> (n `mod` 3) /= 1) naturals
