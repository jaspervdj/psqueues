{-# LANGUAGE BangPatterns #-}

module Data.HashPSQ.Benchmark
    ( benchmark
    ) where

import           Data.List (foldl')
import qualified Data.HashPSQ as HashPSQ
import           Criterion.Main
import           Prelude hiding (lookup)
import           BenchmarkTypes

benchmark :: String -> (Int -> BElem) -> Int -> BenchmarkSet
benchmark name getElem benchmarkSize = BenchmarkSet
    { bGroupName           = name
    , bMinView             = whnf minView' initialPSQ
    , bLookup              = whnf (lookup' keys) initialPSQ
    , bInsertEmpty         = nf (insert' firstElems) HashPSQ.empty
    , bInsertNew           = nf (insert' secondElems) initialPSQ
    , bInsertDuplicates    = nf (insert' firstElems) initialPSQ
    }
  where
    (firstElems, secondElems) = splitAt (benchmarkSize `div` 2) elems
    elems = map getElem [0..benchmarkSize]
    keys = map (\(x,_,_) -> x) elems

    initialPSQ = HashPSQ.fromList firstElems :: HashPSQ.HashPSQ Int Int ()


-- Get the sum of all priorities by getting all elements using 'lookup'
lookup' :: [Int] -> HashPSQ.HashPSQ Int Int () -> Int
lookup' xs m = foldl' (\n k -> maybe n fst (HashPSQ.lookup k m)) 0 xs

-- Insert a list of elements one-by-one into a PSQ
insert' :: [BElem] -> HashPSQ.HashPSQ Int Int () -> HashPSQ.HashPSQ Int Int ()
insert' xs m0 = foldl' (\m (k, p, v) -> HashPSQ.insert k p v m) m0 xs

-- Get the sum of all priorities by sequentially popping all elements using
-- 'minView'
minView' :: HashPSQ.HashPSQ Int Int () -> Int
minView' = go 0
  where
    go !n t = case HashPSQ.minView t of
      Nothing               -> n
      Just ((k, x, _),  t') -> go (n + k + x) t'

