{-# LANGUAGE BangPatterns #-}

-- | This module contains benchmarks for the 'PSQueue' type from the
-- `fingertree-psqueue` package.
module Data.FingerTree.PSQueue.Benchmark
    ( benchmark
    ) where

import           Data.List (foldl')
import           Data.FingerTree.PSQueue hiding (map)
import           Criterion.Main
import           Prelude hiding (lookup)
import           BenchmarkTypes
import           Data.Maybe (fromMaybe)

benchmark :: (Int -> BElem) -> Int -> Benchmark
benchmark getElem benchmarkSize = bgroup "FingerTree PSQueue"
      [ bench "minView" $ whnf prioritySum initialPSQ
      , bench "lookup" $ whnf (lookup' keys) initialPSQ
      , bench "insert (fresh)" $ whnf (insert' elems) empty
      , bench "insert (duplicates)" $ whnf (insert' elems) initialPSQ
      -- , bench "insert (decreasing)" $ whnf (insert' elemsDecreasing) initialPSQ
      , bench "fromList" $ whnf fromList $ map toBinding elems
      ]
  where
    (firstElems, secondElems) = splitAt (benchmarkSize `div` 2) elems
    elems = map getElem [0..benchmarkSize]
    keys = map (\(x,_,_) -> x) elems

    initialPSQ = fromList $ map toBinding firstElems :: PSQ Int Int

    toBinding :: BElem -> Binding Int Int
    toBinding (k, p, v) = k :-> p


lookup' :: [Int] -> PSQ Int Int -> Int
lookup' xs m = foldl' (\n k -> fromMaybe n (lookup k m)) 0 xs

insert' :: [BElem] -> PSQ Int Int -> PSQ Int Int
insert' xs m0 = foldl' (\m (k, p, v) -> fingerInsert k p m) m0 xs
  where
    fingerInsert :: (Ord k, Ord v) => k -> v -> PSQ k v -> PSQ k v
    fingerInsert k v m = alter (const $ Just v) k m

prioritySum :: PSQ Int Int -> Int
prioritySum = go 0
  where
    go !n t = case minView t of
      Nothing              -> n
      Just ((k :-> x), t') -> go (n + k + x) t'

