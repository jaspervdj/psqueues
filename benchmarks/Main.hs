{-# LANGUAGE BangPatterns #-}
module Main where

import           Criterion.Main

import           BenchmarkTypes

import qualified Data.PSQ.Benchmark                 as OrdPSQ
import qualified Data.IntPSQ.Benchmark              as IntPSQ
import qualified Data.WIntPSQ.Benchmark             as WIntPSQ
import qualified Data.HashPSQ.Benchmark             as HashPSQ
import qualified Data.PSQueue.Benchmark             as PSQueue
import qualified Data.FingerTree.PSQueue.Benchmark  as FingerPSQ

main :: IO ()
main = defaultMain (runBenchmark benchmarks)
  where
    getElemsInc x = (x, x, ())
    getElemsDec x = (-x, -x, ())

    benchmarks =
      [ IntPSQ.benchmark "IntPSQ increasing" getElemsInc (2^12)
      , IntPSQ.benchmark "IntPSQ decreasing" getElemsDec (2^12)
      , WIntPSQ.benchmark "WIntPSQ increasing" getElemsInc (2^12)
      , WIntPSQ.benchmark "WIntPSQ decreasing" getElemsDec (2^12)
      , HashPSQ.benchmark "HashPSQ increasing" getElemsInc (2^12)
      , HashPSQ.benchmark "HashPSQ decreasing" getElemsDec (2^12)
      , OrdPSQ.benchmark "OrdPSQ increasing" getElemsInc (2^12)
      , OrdPSQ.benchmark "OrdPSQ decreasing" getElemsDec (2^12)
        -- from the `PSQueue` package
      , PSQueue.benchmark "PSQueue increasing" getElemsInc (2^12)
      , PSQueue.benchmark "PSQueue decreasing" getElemsDec (2^12)
        -- from the `fingertree-psqueues` packagegetElemsDec (2^12)
      , FingerPSQ.benchmark "FingerTree PSQueue increasing" getElemsInc (2^12)
      , FingerPSQ.benchmark "FingerTree PSQueue decreasing" getElemsDec (2^12)
      ]
