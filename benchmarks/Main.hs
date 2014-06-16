{-# LANGUAGE BangPatterns #-}
module Main where

import           Criterion.Main

import           BenchmarkTypes

import qualified Data.OrdPSQ.Benchmark              as OrdPSQ
import qualified Data.IntPSQ.Benchmark              as IntPSQ
import qualified Data.HashPSQ.Benchmark             as HashPSQ
import qualified Data.PSQueue.Benchmark             as PSQueue
import qualified Data.FingerTree.PSQueue.Benchmark  as FingerPSQ

main :: IO ()
main = defaultMain (runBenchmark benchmarks)
  where
    getElemsInc x = (x, x, ())
    getElemsDec x = (-x, -x, ())

    size :: Int
    size = 2 ^ (12 :: Int)

    benchmarks =
      [ IntPSQ.benchmark    "IntPSQ increasing"             getElemsInc size
      , IntPSQ.benchmark    "IntPSQ decreasing"             getElemsDec size
      , HashPSQ.benchmark   "HashPSQ increasing"            getElemsInc size
      , HashPSQ.benchmark   "HashPSQ decreasing"            getElemsDec size
      , OrdPSQ.benchmark    "OrdPSQ increasing"             getElemsInc size
      , OrdPSQ.benchmark    "OrdPSQ decreasing"             getElemsDec size
      , PSQueue.benchmark   "PSQueue increasing"            getElemsInc size
      , PSQueue.benchmark   "PSQueue decreasing"            getElemsDec size
      , FingerPSQ.benchmark "FingerTree PSQueue increasing" getElemsInc size
      , FingerPSQ.benchmark "FingerTree PSQueue decreasing" getElemsDec size
      ]
