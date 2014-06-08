{-# LANGUAGE BangPatterns #-}

module Data.IntPSQ.Benchmark
    ( benchmark
    ) where

import           Data.List (foldl')
import qualified Data.IntPSQ as IntPSQ
import           Criterion.Main
import           Prelude hiding (lookup)
import           BenchmarkTypes

benchmark :: String -> (Int -> BElem) -> Int -> BenchmarkSet
benchmark name getElem benchmarkSize = BenchmarkSet
    { bGroupName        = name
    , bMinView          = whnf minView' initialPSQ
    , bLookup           = whnf (lookup' keys) initialPSQ
    , bInsertEmpty      = nf (insert' firstElems) IntPSQ.empty
    , bInsertNew        = nf (insert' secondElems) initialPSQ
    , bInsertDuplicates = nf (insert' firstElems) initialPSQ
    }
  where
    (firstElems, secondElems) = splitAt (benchmarkSize `div` 2) elems
    elems = map getElem [0..benchmarkSize]
    keys = map (\(x,_,_) -> x) elems

    initialPSQ = IntPSQ.fromList firstElems :: IntPSQ.IntPSQ Int ()

-- Get the sum of all priorities by getting all elements using 'lookup'
lookup' :: [Int] -> IntPSQ.IntPSQ Int () -> Int
lookup' xs m = foldl' (\n k -> maybe n fst (IntPSQ.lookup k m)) 0 xs

-- Insert a list of elements one-by-one into a PSQ
insert' :: [(Int, Int, ())] -> IntPSQ.IntPSQ Int () -> IntPSQ.IntPSQ Int ()
insert' xs m0 = foldl' (\m (k, p, v) -> IntPSQ.insert k p v m) m0 xs

-- Get the sum of all priorities by sequentially popping all elements using
-- 'minView'
minView' :: IntPSQ.IntPSQ Int () -> Int
minView' = go 0
  where
    go !n t = case IntPSQ.minView t of
      Nothing            -> n
      Just (k, p, _, t') -> go (n + k + p) t'
