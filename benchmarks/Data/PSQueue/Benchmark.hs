{-# LANGUAGE BangPatterns #-}

-- | This module provides benchmarks for the 'PSQueue' type from the PSQueue
-- package.
module Data.PSQueue.Benchmark
    ( benchmark
    ) where

import           Data.List (foldl')
import           Data.PSQueue hiding (map)
import           Criterion.Main
import           Prelude hiding (lookup)
import           BenchmarkTypes
import           Data.Maybe (fromMaybe)

benchmark :: (Int -> BElem) -> Int -> Benchmark
benchmark getElem benchmarkSize = bgroup "PSQueue"
    [ bench "minView" $ whnf psqdeleteMins initialPSQ
    , bench "lookup" $ whnf (psqlookup keys) initialPSQ
    , bench "insert (fresh)" $ whnf (psqins elems) empty
    , bench "insert (duplicates)" $ whnf (psqins elems) initialPSQ
    -- , bench "insert (decreasing)" $ whnf (psqins elemsDecreasing) initialPSQ
    , bench "fromList" $ whnf fromList $ map toBinding firstElems
    ]
  where
    (firstElems, secondElems) = splitAt (benchmarkSize `div` 2) elems
    elems = map getElem [0..benchmarkSize]
    keys = map (\(x,_,_) -> x) elems

    initialPSQ = fromList $ map toBinding firstElems :: PSQ Int Int

    toBinding :: BElem -> Binding Int Int
    toBinding (k, p, v) = k :-> p


-- Benchmarking the PSQueues package
-------------------------------------------------------------------------------

psqlookup :: [Int] -> PSQ Int Int -> Int
psqlookup xs m = foldl' (\n k -> fromMaybe n (lookup k m)) 0 xs

psqins :: [BElem] -> PSQ Int Int -> PSQ Int Int
psqins xs m0 = foldl' (\m (k, p, v) -> insert k p m) m0 xs

psqdeleteMins :: PSQ Int Int -> Int
psqdeleteMins = go 0
  where
    go !n t = case minView t of
      Nothing           -> n
      Just ((k :-> x), t') -> go (n + k + x) t'

