{-# LANGUAGE BangPatterns #-}

-- | This module provides benchmarks for the 'PSQueue' type from the PSQueue
-- package.
module Data.PSQueue.Benchmark
    ( benchmark
    ) where

import           Data.List (foldl')
import qualified Data.PSQueue as PSQueue
import           Criterion.Main
import           Prelude hiding (lookup)
import           BenchmarkTypes
import           Data.Maybe (fromMaybe)

benchmark :: String -> (Int -> BElem) -> Int -> BenchmarkSet
benchmark name getElem benchmarkSize = BenchmarkSet
    { bGroupName        = name
    , bMinView          = whnf prioritySum initialPSQ
    , bLookup           = whnf (lookup' keys) initialPSQ
      -- TODO (AS): Get the size of the resulting PSQ, since there's no NFData
      -- instance.
    , bInsertEmpty      = whnf (insert' firstElems) PSQueue.empty
    , bInsertNew        = whnf (insert' secondElems) initialPSQ
    , bInsertDuplicates = whnf (insert' firstElems) initialPSQ
    }
  where
    (firstElems, secondElems) = splitAt (benchmarkSize `div` 2) elems
    elems = map getElem [0..benchmarkSize]
    keys = map (\(x,_,_) -> x) elems

    initialPSQ = PSQueue.fromList $ map toBinding firstElems :: PSQueue.PSQ Int Int

    toBinding :: BElem -> PSQueue.Binding Int Int
    toBinding (k, p, v) = k PSQueue.:-> p


-- Get the sum of all priorities by getting all elements using 'lookup'
lookup' :: [Int] -> PSQueue.PSQ Int Int -> Int
lookup' xs m = foldl' (\n k -> fromMaybe n (PSQueue.lookup k m)) 0 xs

-- Insert a list of elements one-by-one into a PSQ
insert' :: [BElem] -> PSQueue.PSQ Int Int -> PSQueue.PSQ Int Int
insert' xs m0 = foldl' (\m (k, p, v) -> PSQueue.insert k p m) m0 xs

-- Get the sum of all priorities by sequentially popping all elements using
-- 'minView'
prioritySum :: PSQueue.PSQ Int Int -> Int
prioritySum = go 0
  where
    go !n t = case PSQueue.minView t of
      Nothing           -> n
      Just ((k PSQueue.:-> x), t') -> go (n + k + x) t'

