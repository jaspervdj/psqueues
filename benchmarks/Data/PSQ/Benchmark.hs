{-# LANGUAGE BangPatterns #-}

module Data.PSQ.Benchmark
    ( benchmark
    ) where

import           Data.List (foldl')
import           Data.PSQ
import           Criterion.Main
import           Prelude hiding (lookup)
import           BenchmarkTypes

benchmark :: (Int -> BElem) -> Int -> Benchmark
benchmark getElem benchmarkSize = bgroup "OrdPSQ"
    [ bench "minView" $ whnf ord_psqdeleteMins initialPSQ
    , bench "lookup" $ whnf (ord_psqlookup keys) initialPSQ
    , bench "insert (fresh)" $ whnf (ord_psqins elems) empty
    , bench "insert (next fresh)" $ whnf (ord_psqins secondElems) initialPSQ
    , bench "insert (duplicates)" $ whnf (ord_psqins elems) initialPSQ
    -- , bench "insert (decreasing)" $ whnf (ord_psqins elemsDecreasing) initialPSQ
    ]
  where
    (firstElems, secondElems) = splitAt (benchmarkSize `div` 2) elems
    elems = map getElem [0..benchmarkSize]
    keys = map (\(x,_,_) -> x) elems

    initialPSQ = fromList firstElems :: PSQ Int Int ()

-- Benchmarking our PSQ type
-------------------------------------------------------------------------------

ord_psqlookup :: [Int] -> PSQ Int Int () -> Int
ord_psqlookup xs m = foldl' (\n k -> maybe n fst (lookup k m)) 0 xs

ord_psqins :: [BElem] -> PSQ Int Int () -> PSQ Int Int ()
ord_psqins xs m0 = foldl' (\m (k, p, v) -> insert k p v m) m0 xs

ord_psqdeleteMins :: PSQ Int Int () -> Int
ord_psqdeleteMins = go 0
  where
    go !n t = case minView t of
      Nothing              -> n
      Just ((k, x, _), t') -> go (n + k + x) t'
