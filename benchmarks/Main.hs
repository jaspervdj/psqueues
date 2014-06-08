{-# LANGUAGE BangPatterns #-}
module Main where

import BenchmarkTypes

import           Control.DeepSeq
import           Control.Exception (evaluate)
import           Control.Monad.Trans (liftIO)

import           Criterion.Config
import           Criterion.Main

import           Data.List (foldl')
import           Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict     as HashMap
import qualified Data.IntMap.Strict      as IntMap
import qualified Data.Map.Strict         as Map

import qualified Data.PSQ.Benchmark                 as OrdPSQ
import qualified Data.IntPSQ.Benchmark              as IntPSQ
import qualified Data.HashPSQ.Benchmark             as HashPSQ
import qualified Data.PSQueue.Benchmark             as PSQueue
import qualified Data.FingerTree.PSQueue.Benchmark  as FingerPSQ

import           Prelude hiding (lookup)


main :: IO ()
main = do
    defaultMainWith defaultConfig (liftIO forceData) (runBenchmark benchmarks)
    -- retain a reference to all data until after the benchmarks to ensure
    -- that all benchmarks have the same GC costs
    forceData
  where
    -- data definition
    keys   = [1..2^(12 :: Int)] :: [Int]
    values = [1..2^(12 :: Int)] :: [Int]

    getElemsInc x = (x, x, ())
    getElemsDec x = (-x, -x, ())

    elems               = zip keys values
    elems_with_unit     = [ (k,p,()) | (k,p) <- elems ]
    nextElems           = [ (k + 2^(12 + 1 :: Int), p) | (k, p) <- elems ]
    nextElems_with_unit = [ (k,p,()) | (k,p) <- nextElems ]

    allElems = [ (k,p,()) | (k,p) <- elems ++ nextElems ]

    elemsDecreasing = zip (reverse keys) values

    hms      = HashMap.fromList elems           :: HashMap.HashMap Int Int
    ms       = Map.fromList     elems           :: Map.Map Int Int
    ims      = IntMap.fromList  elems           :: IntMap.IntMap Int

    -- forcing the data
    forceData = do
        evaluate (rnf hms)
        evaluate (rnf ms)
        evaluate (rnf ims)
        return ()

    benchmarks =
      [ OrdPSQ.benchmark "OrdPSQ increasing" getElemsInc (2^12)
      , OrdPSQ.benchmark "OrdPSQ decreasing" getElemsDec (2^12)
      , IntPSQ.benchmark "IntPSQ increasing" getElemsInc (2^12)
      , IntPSQ.benchmark "IntPSQ decreasing" getElemsDec (2^12)
      , HashPSQ.benchmark "HashPSQ increasing" getElemsInc (2^12)
      , HashPSQ.benchmark "HashPSQ decreasing" getElemsDec (2^12)
        -- from the `PSQueue` package
      , PSQueue.benchmark "PSQueue increasing" getElemsInc (2^12)
      , PSQueue.benchmark "PSQueue decreasing" getElemsDec (2^12)
        -- from the `fingertree-psqueues` packagegetElemsDec (2^12)
      , FingerPSQ.benchmark "FingerTree PSQueue increasing" getElemsInc (2^12)
      , FingerPSQ.benchmark "FingerTree PSQueue decreasing" getElemsDec (2^12)
      ]

    {-
    -- old benchmarks
    benchmarks =
      [ bench_intmap
      , bench_hashmap
      , bench_map
      ]
    -}

    bench_hashmap = bgroup "HashMap"
      [ bench "lookup" $ whnf (hashmap_lookup keys) hms
      , bench "insert (fresh)" $ whnf (hashmap_ins elems) HashMap.empty
      ]

    bench_map = bgroup "Map"
      [ bench "lookup" $ whnf (map_lookup keys) ms
      , bench "insert (fresh)" $ whnf (map_ins elems) Map.empty
      ]

    bench_intmap = bgroup "IntMap"
      [ bench "lookup" $ whnf (intmap_lookup keys) ims
      , bench "insert (fresh)" $ whnf (intmap_ins elems) IntMap.empty
      ]


hashmap_lookup :: [Int] -> HashMap.HashMap Int Int -> Int
hashmap_lookup xs m = foldl' (\n k -> fromMaybe n (HashMap.lookup k m)) 0 xs

map_lookup :: [Int] -> Map.Map Int Int -> Int
map_lookup xs m = foldl' (\n k -> fromMaybe n (Map.lookup k m)) 0 xs

intmap_lookup :: [Int] -> IntMap.IntMap Int -> Int
intmap_lookup xs m = foldl' (\n k -> fromMaybe n (IntMap.lookup k m)) 0 xs

hashmap_ins :: [(Int, Int)] -> HashMap.HashMap Int Int -> HashMap.HashMap Int Int
hashmap_ins xs m0 = foldl' (\m (k, v) -> HashMap.insert k v m) m0 xs

map_ins :: [(Int, Int)] -> Map.Map Int Int -> Map.Map Int Int
map_ins xs m0 = foldl' (\m (k, v) -> Map.insert k v m) m0 xs

intmap_ins :: [(Int, Int)] -> IntMap.IntMap Int -> IntMap.IntMap Int
intmap_ins xs m0 = foldl' (\m (k, v) -> IntMap.insert k v m) m0 xs

