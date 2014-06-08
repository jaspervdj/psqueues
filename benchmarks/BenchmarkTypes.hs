
module BenchmarkTypes where

import Criterion

type BElem = (Int, Int, ())

data BenchmarkSet = BenchmarkSet
    { bGroupName        :: String
    , bMinView          :: Pure
    , bLookup           :: Pure
    , bInsertEmpty      :: Pure
    , bInsertNew        :: Pure
    , bInsertDuplicates :: Pure
    }

runBenchmark :: [BenchmarkSet] -> [Benchmark]
runBenchmark bset =
    [ bgroup "minView"          $ map (bench' bMinView) bset
    , bgroup "lookup"           $ map (bench' bLookup) bset
    , bgroup "insertEmpty"      $ map (bench' bInsertEmpty) bset
    , bgroup "insertNew"        $ map (bench' bInsertNew) bset
    , bgroup "insertDuplicates" $ map (bench' bInsertDuplicates) bset
    ]
  where
   bench' f x = bench (bGroupName x) (f x)
