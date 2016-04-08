
module BenchmarkTypes where

import Criterion

type BElem = (Int, Int, ())

data BenchmarkSet = BenchmarkSet
    { bGroupName        :: String
    , bMinView          :: Benchmarkable
    , bLookup           :: Benchmarkable
    , bInsertEmpty      :: Benchmarkable
    , bInsertNew        :: Benchmarkable
    , bInsertDuplicates :: Benchmarkable
    , bDelete           :: Benchmarkable
    }

runBenchmark :: [BenchmarkSet] -> [Benchmark]
runBenchmark bset =
    [ bgroup "minView"          $ map (bench' bMinView)          bset
    , bgroup "lookup"           $ map (bench' bLookup)           bset
    , bgroup "insertEmpty"      $ map (bench' bInsertEmpty)      bset
    , bgroup "insertNew"        $ map (bench' bInsertNew)        bset
    , bgroup "insertDuplicates" $ map (bench' bInsertDuplicates) bset
    , bgroup "delete"           $ map (bench' bDelete)           bset
    ]
  where
   bench' f x = bench (bGroupName x) (f x)
