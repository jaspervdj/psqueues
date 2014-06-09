{-# LANGUAGE BangPatterns #-}

module Data.WIntPSQ.Benchmark
    ( benchmark
    ) where

import           Data.List (foldl')
import qualified Data.WIntPSQ as WIntPSQ
import           Criterion.Main
import           Prelude hiding (lookup)
import           BenchmarkTypes

benchmark :: String -> (Int -> BElem) -> Int -> BenchmarkSet
benchmark name getElem benchmarkSize = BenchmarkSet
    { bGroupName        = name
    , bMinView          = whnf minView' initialPSQ
    , bLookup           = whnf (lookup' keys) initialPSQ
    , bInsertEmpty      = nf (insert' firstElems) WIntPSQ.empty
    , bInsertNew        = nf (insert' secondElems) initialPSQ
    , bInsertDuplicates = nf (insert' firstElems) initialPSQ
    }
  where
    (firstElems, secondElems) = splitAt (benchmarkSize `div` 2) elems
    elems = map getElem [0..benchmarkSize]
    keys = map (\(x,_,_) -> x) elems

    initialPSQ = WIntPSQ.fromList firstElems :: WIntPSQ.WIntPSQ Int ()

-- Get the sum of all priorities by getting all elements using 'lookup'
lookup' :: [Int] -> WIntPSQ.WIntPSQ Int () -> Int
lookup' xs m = foldl' (\n k -> maybe n fst (WIntPSQ.lookup k m)) 0 xs

-- Insert a list of elements one-by-one into a PSQ
insert' :: [(Int, Int, ())] -> WIntPSQ.WIntPSQ Int () -> WIntPSQ.WIntPSQ Int ()
insert' xs m0 = foldl' (\m (k, p, v) -> WIntPSQ.insert k p v m) m0 xs

-- Get the sum of all priorities by sequentially popping all elements using
-- 'minView'
minView' :: WIntPSQ.WIntPSQ Int () -> Int
minView' = go 0
  where
    go !n t = case WIntPSQ.minView t of
      Nothing            -> n
      Just (k, p, _, t') -> go (n + k + p) t'
