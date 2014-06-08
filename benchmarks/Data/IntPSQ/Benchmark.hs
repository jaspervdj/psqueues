{-# LANGUAGE BangPatterns #-}

module Data.IntPSQ.Benchmark
    ( benchmark
    ) where

import           Data.List (foldl')
import           Data.IntPSQ hiding (map)
import           Criterion.Main
import           Prelude hiding (lookup)
import           BenchmarkTypes

benchmark :: (Int -> BElem) -> Int -> Benchmark
benchmark getElem benchmarkSize = bgroup "IntPSQ"
      [ bench "minView" $ whnf deleteMins initialPSQ
      , bench "lookup" $ whnf (lookup' keys) initialPSQ
      , bench "insert (fresh)" $ whnf (ins firstElems) empty
      , bench "insert (next fresh)" $ whnf (ins secondElems) initialPSQ
      , bench "insert (duplicates)" $ whnf (ins firstElems) initialPSQ
      -- , bench "insert (decreasing)" $ whnf (ins elemsDecreasing) initialPSQ
      -- , bench "fromList" $ whnf fromList elems_with_unit
      ]
  where
    (firstElems, secondElems) = splitAt (benchmarkSize `div` 2) elems
    elems = map getElem [0..benchmarkSize]
    keys = map (\(x,_,_) -> x) elems

    initialPSQ = fromList firstElems :: IntPSQ Int ()

lookup' :: [Int] -> IntPSQ Int () -> Int
lookup' xs m = foldl' (\n k -> maybe n fst (lookup k m)) 0 xs

ins :: [(Int, Int, ())] -> IntPSQ Int () -> IntPSQ Int ()
ins xs m0 = foldl' (\m (k, p, v) -> insert k p v m) m0 xs

deleteMins :: IntPSQ Int () -> Int
deleteMins = go 0
  where
    go !n t = case minView t of
      Nothing            -> n
      Just (k, p, _, t') -> go (n + k + p) t'
