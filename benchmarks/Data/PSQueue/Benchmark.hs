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

benchmark :: String -> [BElem] -> BenchmarkSet
benchmark name elems = BenchmarkSet
    { bGroupName        = name
    , bMinView          = whnf bench_minView              initialPSQ
    , bLookup           = whnf (bench_lookup keys)        initialPSQ
    , bInsertEmpty      = nf'  (bench_insert firstElems)  PSQueue.empty
    , bInsertNew        = nf'  (bench_insert secondElems) initialPSQ
    , bInsertDuplicates = nf'  (bench_insert firstElems)  initialPSQ
    , bDelete           = nf'  (bench_delete firstKeys)   initialPSQ
    }
  where
    (firstElems, secondElems) = splitAt (numElems `div` 2) elems
    numElems  = length elems
    keys      = map (\(x, _, _) -> x) elems
    firstKeys = map (\(x, _, _) -> x) firstElems

    initialPSQ
        = PSQueue.fromList $ map toBinding firstElems :: PSQueue.PSQ Int Int

    toBinding :: BElem -> PSQueue.Binding Int Int
    toBinding (k, p, _) = k PSQueue.:-> p

    -- Get the size of the resulting PSQ, since there's no NFData instance.
    nf' f x = whnf (PSQueue.size . f) x

-- Get the sum of all priorities by getting all elements using 'lookup'
bench_lookup :: [Int] -> PSQueue.PSQ Int Int -> Int
bench_lookup xs m = foldl' (\n k -> fromMaybe n (PSQueue.lookup k m)) 0 xs

-- Insert a list of elements one-by-one into a PSQ
bench_insert :: [BElem] -> PSQueue.PSQ Int Int -> PSQueue.PSQ Int Int
bench_insert xs m0 = foldl' (\m (k, p, _) -> PSQueue.insert k p m) m0 xs

-- Get the sum of all priorities by sequentially popping all elements using
-- 'minView'
bench_minView :: PSQueue.PSQ Int Int -> Int
bench_minView = go 0
  where
    go !n t = case PSQueue.minView t of
      Nothing           -> n
      Just ((k PSQueue.:-> x), t') -> go (n + k + x) t'

-- Empty a queue by sequentially removing all elements
bench_delete :: [Int] -> PSQueue.PSQ Int Int -> PSQueue.PSQ Int Int
bench_delete keys t0 = foldl' (\t k -> PSQueue.delete k t) t0 keys
