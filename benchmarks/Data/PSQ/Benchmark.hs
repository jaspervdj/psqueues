{-# LANGUAGE BangPatterns #-}

module Data.PSQ.Benchmark
    ( benchmark
    ) where

import           Data.List (foldl')
import qualified Data.PSQ as PSQ
import           Criterion.Main
import           Prelude hiding (lookup)
import           BenchmarkTypes

benchmark :: String -> (Int -> BElem) -> Int -> BenchmarkSet
benchmark name getElem benchmarkSize = BenchmarkSet
    { bGroupName        = name
    , bMinView          = whnf minView' initialPSQ
    , bLookup           = whnf (lookup' keys) initialPSQ
    , bInsertEmpty      = nf (insert' firstElems) PSQ.empty
    , bInsertNew        = nf (insert' secondElems) initialPSQ
    , bInsertDuplicates = nf (insert' firstElems) initialPSQ
    }
  where
    (firstElems, secondElems) = splitAt (benchmarkSize `div` 2) elems
    elems = map getElem [0..benchmarkSize]
    keys = map (\(x,_,_) -> x) elems

    initialPSQ = PSQ.fromList firstElems :: PSQ.PSQ Int Int ()


-- Get the sum of all priorities by getting all elements using 'lookup'
lookup' :: [Int] -> PSQ.PSQ Int Int () -> Int
lookup' xs m = foldl' (\n k -> maybe n fst (PSQ.lookup k m)) 0 xs

-- Insert a list of elements one-by-one into a PSQ
insert' :: [BElem] -> PSQ.PSQ Int Int () -> PSQ.PSQ Int Int ()
insert' xs m0 = foldl' (\m (k, p, v) -> PSQ.insert k p v m) m0 xs

-- Get the sum of all priorities by sequentially popping all elements using
-- 'minView'
minView' :: PSQ.PSQ Int Int () -> Int
minView' = go 0
  where
    go !n t = case PSQ.minView t of
      Nothing            -> n
      Just (k, x, _, t') -> go (n + k + x) t'
