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
    [ bench "minView" $ whnf prioritySum initialPSQ
    , bench "lookup" $ whnf (lookup' keys) initialPSQ
    , bench "insert (fresh)" $ whnf (insert' elems) empty
    , bench "insert (duplicates)" $ whnf (insert' elems) initialPSQ
    -- , bench "insert (decreasing)" $ whnf (insert' elemsDecreasing) initialPSQ
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

lookup' :: [Int] -> PSQ Int Int -> Int
lookup' xs m = foldl' (\n k -> fromMaybe n (lookup k m)) 0 xs

insert' :: [BElem] -> PSQ Int Int -> PSQ Int Int
insert' xs m0 = foldl' (\m (k, p, v) -> insert k p m) m0 xs

prioritySum :: PSQ Int Int -> Int
prioritySum = go 0
  where
    go !n t = case minView t of
      Nothing           -> n
      Just ((k :-> x), t') -> go (n + k + x) t'

