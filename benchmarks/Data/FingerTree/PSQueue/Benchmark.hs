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

benchmark :: String -> (Int -> BElem) -> Int -> BenchmarkSet
benchmark name getElem benchmarkSize = BenchmarkSet
    { bGroupName = name
    , bMinView                = whnf minView' initialPSQ
    , bLookup                 = whnf (lookup' keys) initialPSQ
      -- TODO (AS): get the size of the resulting PSQs, since there's no
      -- NFData instance
    , bInsertEmpty            = whnf (insert' firstElems) empty
    , bInsertNew              = whnf (insert' secondElems) initialPSQ
    , bInsertDuplicates       = whnf (insert' firstElems) initialPSQ
    }
  where
    (firstElems, secondElems) = splitAt (benchmarkSize `div` 2) elems
    elems = map getElem [0..benchmarkSize]
    keys = map (\(x,_,_) -> x) elems

    initialPSQ = fromList $ map toBinding firstElems :: PSQ Int Int

    toBinding :: BElem -> Binding Int Int
    toBinding (k, p, v) = k :-> p


lookup' :: [Int] -> PSQ Int Int -> Int
lookup' xs m = foldl' (\n k -> fromMaybe n (lookup k m)) 0 xs

insert' :: [BElem] -> PSQ Int Int -> PSQ Int Int
insert' xs m0 = foldl' (\m (k, p, v) -> fingerInsert k p m) m0 xs
  where
    fingerInsert :: (Ord k, Ord v) => k -> v -> PSQ k v -> PSQ k v
    fingerInsert k v m = alter (const $ Just v) k m

minView' :: PSQ Int Int -> Int
minView' = go 0
  where
    go !n t = case minView t of
      Nothing              -> n
      Just ((k :-> x), t') -> go (n + k + x) t'

