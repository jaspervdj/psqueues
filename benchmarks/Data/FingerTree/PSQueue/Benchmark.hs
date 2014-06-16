{-# LANGUAGE BangPatterns #-}

-- | This module contains benchmarks for the 'PSQueue' type from the
-- `fingertree-psqueue` package.
module Data.FingerTree.PSQueue.Benchmark
    ( benchmark
    ) where

import           Data.List (foldl')
import           Data.FingerTree.PSQueue (Binding (..))
import qualified Data.FingerTree.PSQueue as PSQueue
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

    initialPSQ :: PSQueue.PSQ Int Int
    initialPSQ = PSQueue.fromList $ map toBinding firstElems

    toBinding :: BElem -> Binding Int Int
    toBinding (k, p, _) = k :-> p

    -- Get the size of the resulting PSQs, since there's no NFData instance
    nf' f x = whnf (PSQueue.size . f) x

bench_lookup :: [Int] -> PSQueue.PSQ Int Int -> Int
bench_lookup xs m = foldl' (\n k -> fromMaybe n (PSQueue.lookup k m)) 0 xs

bench_insert :: [BElem] -> PSQueue.PSQ Int Int -> PSQueue.PSQ Int Int
bench_insert xs m0 = foldl' (\m (k, p, _) -> fingerInsert k p m) m0 xs
  where
    fingerInsert
        :: (Ord k, Ord v) => k -> v -> PSQueue.PSQ k v -> PSQueue.PSQ k v
    fingerInsert k v m = PSQueue.alter (const $ Just v) k m

bench_minView :: PSQueue.PSQ Int Int -> Int
bench_minView = go 0
  where
    go !n t = case PSQueue.minView t of
      Nothing              -> n
      Just ((k :-> x), t') -> go (n + k + x) t'

-- Empty a queue by sequentially removing all elements
bench_delete :: [Int] -> PSQueue.PSQ Int Int -> PSQueue.PSQ Int Int
bench_delete keys t0 = foldl' (\t k -> PSQueue.delete k t) t0 keys
