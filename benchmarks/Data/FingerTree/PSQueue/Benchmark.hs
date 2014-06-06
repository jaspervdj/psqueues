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
      [ bench "minView" $ whnf fingerDeleteMins initialPSQ
      , bench "lookup" $ whnf (fingerLookup keys) initialPSQ
      , bench "insert (fresh)" $ whnf (fingerIns elems) empty
      , bench "insert (duplicates)" $ whnf (fingerIns elems) initialPSQ
      -- , bench "insert (decreasing)" $ whnf (fingerIns elemsDecreasing) initialPSQ
      , bench "fromList" $ whnf fromList $ map toBinding elems
      ]
  where
    (firstElems, secondElems) = splitAt (benchmarkSize `div` 2) elems
    elems = map getElem [0..benchmarkSize]
    keys = map (\(x,_,_) -> x) elems

    initialPSQ = fromList $ map toBinding firstElems :: PSQ Int Int

    toBinding :: BElem -> Binding Int Int
    toBinding (k, p, v) = k :-> p

-- Benchmarking the psqueue from the fingertree-psqueue package
-------------------------------------------------------------------------------

fingerLookup :: [Int] -> PSQ Int Int -> Int
fingerLookup xs m = foldl' (\n k -> fromMaybe n (lookup k m)) 0 xs

fingerIns :: [BElem] -> PSQ Int Int -> PSQ Int Int
fingerIns xs m0 = foldl' (\m (k, p, v) -> fingerInsert k p m) m0 xs
  where
    fingerInsert :: (Ord k, Ord v) => k -> v -> PSQ k v -> PSQ k v
    fingerInsert k v m = alter (const $ Just v) k m

fingerDeleteMins :: PSQ Int Int -> Int
fingerDeleteMins = go 0
  where
    go !n t = case minView t of
      Nothing           -> n
      Just ((k :-> x), t') -> go (n + k + x) t'

