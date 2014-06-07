{-# LANGUAGE BangPatterns #-}

module Data.HashPSQ.Benchmark
    ( benchmark
    ) where

import           Data.List (foldl')
import           Data.HashPSQ hiding (map)
import           Criterion.Main
import           Prelude hiding (lookup)
import           BenchmarkTypes

benchmark :: (Int -> BElem) -> Int -> Benchmark
benchmark getElem benchmarkSize = bgroup "HashPSQ"
      [ bench "minView" $ whnf prioritySum hash_psq
      , bench "lookup" $ whnf (lookup' keys) hash_psq
      , bench "insert (fresh)" $ whnf (insert' elems) empty
      , bench "insert (next fresh)" $ whnf (insert' secondElems) hash_psq
      , bench "insert (duplicates)" $ whnf (insert' elems) hash_psq
      -- , bench "insert (decreasing)" $ whnf (insert' elemsDecreasing) hash_psq
      ]
  where
    (firstElems, secondElems) = splitAt (benchmarkSize `div` 2) elems
    elems = map getElem [0..benchmarkSize]
    keys = map (\(x,_,_) -> x) elems

    hash_psq = fromList firstElems :: HashPSQ Int Int ()


lookup' :: [Int] -> HashPSQ Int Int () -> Int
lookup' xs m = foldl' (\n k -> maybe n fst (lookup k m)) 0 xs

insert' :: [BElem] -> HashPSQ Int Int () -> HashPSQ Int Int ()
insert' xs m0 = foldl' (\m (k, p, v) -> insert k p v m) m0 xs

prioritySum :: HashPSQ Int Int () -> Int
prioritySum = go 0
  where
    go !n t = case minView t of
      Nothing               -> n
      Just ((k, x, _),  t') -> go (n + k + x) t'

