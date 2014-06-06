{-# LANGUAGE BangPatterns #-}

module Data.IntPSQ.Benchmark
    ( benchmark
    ) where

import           Data.List (foldl')
import           Data.IntPSQ hiding (map)
import           Criterion.Main
import           Prelude hiding (lookup)
import           BenchmarkTypes

benchmark :: (Int -> Elem) -> Int -> Benchmark
benchmark getElem benchmarkSize = bgroup "IntPSQ"
      [ bench "minView" $ whnf deleteMins initialPSQ
      -- , bench "map (id)" $ whnf (IntPSQ.map id) int_psq
      -- , bench "map (negate)" $ whnf (IntPSQ.map negate) int_psq
      , bench "lookup" $ whnf (lookup' keys) initialPSQ

      , bench "insert (fresh)" $ whnf (ins firstElems) empty
      , bench "insert (next fresh)" $ whnf (ins secondElems) initialPSQ
      , bench "insert (duplicates)" $ whnf (ins firstElems) initialPSQ
      -- , bench "insert (decreasing)" $ whnf (ins elemsDecreasing) initialPSQ
      -- , bench "fromList" $ whnf fromList elems_with_unit

      , bench "insert2 (fresh)" $ whnf (ins2 firstElems) empty
      , bench "insert2 (duplicates)" $ whnf (ins2 firstElems) initialPSQ
      -- , bench "insert2 (decreasing)" $ whnf (ins2 elemsDecreasing) initialPSQ
      , bench "fromList2" $ whnf fromList2 elems

      , bench "insert3 (fresh)" $ whnf (ins3 firstElems) empty
      , bench "insert3 (duplicates)" $ whnf (ins3 firstElems) initialPSQ
      -- , bench "insert3 (decreasing)" $ whnf (ins3 elemsDecreasing) initialPSQ
      , bench "fromList3" $ whnf fromList3 elems
      ]
  where
    (firstElems, secondElems) = splitAt (benchmarkSize `div` 2) elems
    elems = map getElem [0..benchmarkSize]
    keys = map (\(x,_,_) -> x) elems

    initialPSQ = fromList firstElems :: IntPSQ Int ()



-- Benchmarking our IntPSQ type
-------------------------------------------------------------------------------

lookup' :: [Int] -> IntPSQ Int () -> Int
lookup' xs m = foldl' (\n k -> maybe n fst (lookup k m)) 0 xs

ins :: [(Int, Int, ())] -> IntPSQ Int () -> IntPSQ Int ()
ins xs m0 = foldl' (\m (k, p, v) -> insert k p v m) m0 xs

ins2 :: [(Int, Int, ())] -> IntPSQ Int () -> IntPSQ Int ()
ins2 xs m0 = foldl' (\m (k, p, v) -> insert2 k p v m) m0 xs

ins3 :: [(Int, Int, ())] -> IntPSQ Int () -> IntPSQ Int ()
ins3 xs m0 = foldl' (\m (k, p, v) -> insert3 k p v m) m0 xs

deleteMins :: IntPSQ Int () -> Int
deleteMins = go 0
  where
    go !n t = case minViewWithKey t of
      Nothing           -> n
      Just ((k, p, _), t') -> go (n + k + p) t'
