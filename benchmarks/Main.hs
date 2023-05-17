{-# LANGUAGE BangPatterns #-}
module Main where

import           Criterion.Main
import           System.Random

import           BenchmarkTypes

import qualified Data.OrdPSQ.Benchmark              as OrdPSQ
import qualified Data.IntPSQ.Benchmark              as IntPSQ
import qualified Data.HashPSQ.Benchmark             as HashPSQ
import qualified Data.PSQueue.Benchmark             as PSQueue

benchmarkSize :: Int
benchmarkSize = 2 ^ (12 :: Int)

{-# NOINLINE increasing #-}
increasing :: [BElem]
increasing = [(n, n, ()) | n <- [1 .. benchmarkSize]]

{-# NOINLINE decreasing #-}
decreasing :: [BElem]
decreasing = reverse increasing

{-# NOINLINE semirandom #-}
semirandom :: [BElem]
semirandom =
    [ (x, y, ())
    | (_, x, y) <- zip3 [1 .. benchmarkSize] (randoms gen1) (randoms gen2)
    ]
  where
    gen1 = mkStdGen 1234
    gen2 = mkStdGen 5678

main :: IO ()
main = defaultMain $ runBenchmark
    [ IntPSQ.benchmark    "IntPSQ increasing"             increasing
    , IntPSQ.benchmark    "IntPSQ decreasing"             decreasing
    , IntPSQ.benchmark    "IntPSQ semirandom"             semirandom
    , HashPSQ.benchmark   "HashPSQ increasing"            increasing
    , HashPSQ.benchmark   "HashPSQ decreasing"            decreasing
    , HashPSQ.benchmark   "HashPSQ semirandom"            semirandom
    , OrdPSQ.benchmark    "OrdPSQ increasing"             increasing
    , OrdPSQ.benchmark    "OrdPSQ decreasing"             decreasing
    , OrdPSQ.benchmark    "OrdPSQ semirandom"             semirandom
    , PSQueue.benchmark   "PSQueue increasing"            increasing
    , PSQueue.benchmark   "PSQueue decreasing"            decreasing
    , PSQueue.benchmark   "PSQueue semirandom"            semirandom
    ]
