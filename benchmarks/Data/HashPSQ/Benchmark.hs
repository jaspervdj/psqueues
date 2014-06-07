{-# LANGUAGE BangPatterns #-}

module Data.HashPSQ.Benchmark
    ( benchmark
    ) where

import           Data.List (foldl')
import           Data.HashPSQ hiding (map)
import           Criterion.Main
import           Prelude hiding (lookup)
import           BenchmarkTypes

benchmark :: (Int -> Elem) -> Int -> Benchmark
benchmark getElem benchmarkSize = bgroup "HashPSQ"
      [ bench "minView" $ whnf hash_psqdeleteMins hash_psq
      , bench "lookup" $ whnf (hash_psqlookup keys) hash_psq
      , bench "insert (fresh)" $ whnf (hash_psqins elems) empty
      , bench "insert (next fresh)" $ whnf (hash_psqins secondElems) hash_psq
      , bench "insert (duplicates)" $ whnf (hash_psqins elems) hash_psq
      -- , bench "insert (decreasing)" $ whnf (hash_psqins elemsDecreasing) hash_psq
      ]
  where
    (firstElems, secondElems) = splitAt (benchmarkSize `div` 2) elems
    elems = map getElem [0..benchmarkSize]
    keys = map (\(x,_,_) -> x) elems

    hash_psq = fromList firstElems :: HashPSQ Int Int ()

-- Benchmarking our HashPSQ type
-------------------------------------------------------------------------------

hash_psqlookup :: [Int] -> HashPSQ Int Int () -> Int
hash_psqlookup xs m = foldl' (\n k -> maybe n fst (lookup k m)) 0 xs

hash_psqins :: [Elem] -> HashPSQ Int Int () -> HashPSQ Int Int ()
hash_psqins xs m0 = foldl' (\m (k, p, v) -> insert k p v m) m0 xs

hash_psqdeleteMins :: HashPSQ Int Int () -> Int
hash_psqdeleteMins = go 0
  where
    go !n t = case minView t of
      Nothing               -> n
      Just ((k, x, _),  t') -> go (n + k + x) t'

