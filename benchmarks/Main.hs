{-# LANGUAGE BangPatterns #-}
module Main where

import           Control.DeepSeq
import           Control.Exception (evaluate)
import           Control.Monad.Trans (liftIO)

import           Criterion.Config
import           Criterion.Main

import           Data.List (foldl')
import           Data.Maybe (fromMaybe)
import qualified Data.IntPSQ             as IntPSQ
import qualified Data.PSQ                as OrdPSQ
import qualified Data.HashPSQ            as HashPSQ
import qualified Data.HashMap.Strict     as HashMap
import qualified Data.IntMap.Strict      as IntMap
import qualified Data.Map.Strict         as Map
import qualified Data.PSQueue            as PSQueue
import qualified Data.FingerTree.PSQueue as FingerPSQ

import qualified Data.IntPSQ.Benchmark   as IntPSQ
import qualified Data.HashPSQ.Benchmark   as HashPSQ

import           Prelude hiding (lookup)


main :: IO ()
main = do
    defaultMainWith defaultConfig (liftIO forceData) benchmarks
    -- retain a reference to all data until after the benchmarks to ensure
    -- that all benchmarks have the same GC costs
    forceData
  where
    -- data definition
    keys   = [1..2^(12 :: Int)] :: [Int]
    values = [1..2^(12 :: Int)] :: [Int]

    getElems x = (x, x, ())

    elems               = zip keys values
    elems_with_unit     = [ (k,p,()) | (k,p) <- elems ]
    nextElems           = [ (k + 2^(12 + 1 :: Int), p) | (k, p) <- elems ]
    nextElems_with_unit = [ (k,p,()) | (k,p) <- nextElems ]

    allElems = [ (k,p,()) | (k,p) <- elems ++ nextElems ]

    elemsDecreasing = zip (reverse keys) values
    psqelems        = map (uncurry (PSQueue.:->)) elems
    fingerElems     = map (uncurry (FingerPSQ.:->)) elems
    ord_psqelems    = map (\(k,p) -> OrdPSQ.E k p ()) elems

    hms      = HashMap.fromList elems           :: HashMap.HashMap Int Int
    ms       = Map.fromList     elems           :: Map.Map Int Int
    ims      = IntMap.fromList  elems           :: IntMap.IntMap Int

    psq      = PSQueue.fromList psqelems        :: PSQueue.PSQ Int Int
    ord_psq  = OrdPSQ.fromList ord_psqelems    :: OrdPSQ.PSQ Int Int ()
    finger   = FingerPSQ.fromList fingerElems   :: FingerPSQ.PSQ Int Int

    -- forcing the data
    forceData = do
        evaluate (rnf hms)
        evaluate (rnf ms)
        evaluate (rnf ims)
        evaluate (rnf [ (x,y) | (x PSQueue.:-> y) <- PSQueue.toList psq])
        evaluate (rnf [ (x,y,z) | (OrdPSQ.E x y z) <- OrdPSQ.toList ord_psq])
        return ()

    -- benchmarks
    benchmarks =
      [ bench_ghc_events_psq
      , IntPSQ.benchmark getElems (2^12)
      , HashPSQ.benchmark getElems (2^12)
      , bench_psqueue
      , bench_finger

      , bench_intmap
      , bench_hashmap
      , bench_map
      ]

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

    bench_ghc_events_psq = bgroup "GHC_Events_PSQ"
      [ bench "minView" $ whnf ord_psqdeleteMins ord_psq
      , bench "lookup" $ whnf (ord_psqlookup keys) ord_psq
      , bench "insert (fresh)" $ whnf (ord_psqins elems) OrdPSQ.empty
      , bench "insert (next fresh)" $ whnf (ord_psqins nextElems) ord_psq
      , bench "insert (duplicates)" $ whnf (ord_psqins elems) ord_psq
      , bench "insert (decreasing)" $ whnf (ord_psqins elemsDecreasing) ord_psq
      ]


    bench_psqueue = bgroup "PSQueue"
      [ bench "minView" $ whnf psqdeleteMins psq
      , bench "lookup" $ whnf (psqlookup keys) psq
      , bench "insert (fresh)" $ whnf (psqins elems) PSQueue.empty
      , bench "insert (duplicates)" $ whnf (psqins elems) psq
      , bench "insert (decreasing)" $ whnf (psqins elemsDecreasing) psq
      , bench "fromList" $ whnf PSQueue.fromList psqelems
      ]

    bench_finger = bgroup "FingerTree PSQueue"
      [ bench "minView" $ whnf fingerDeleteMins finger
      , bench "lookup" $ whnf (fingerLookup keys) finger
      , bench "insert (fresh)" $ whnf (fingerIns elems) FingerPSQ.empty
      , bench "insert (duplicates)" $ whnf (fingerIns elems) finger
      , bench "insert (decreasing)" $ whnf (fingerIns elemsDecreasing) finger
      , bench "fromList" $ whnf FingerPSQ.fromList fingerElems
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



-- Benchmarking the PSQueues package
-------------------------------------------------------------------------------

psqlookup :: [Int] -> PSQueue.PSQ Int Int -> Int
psqlookup xs m = foldl' (\n k -> fromMaybe n (PSQueue.lookup k m)) 0 xs

psqins :: [(Int, Int)] -> PSQueue.PSQ Int Int -> PSQueue.PSQ Int Int
psqins xs m0 = foldl' (\m (k, v) -> PSQueue.insert k v m) m0 xs

psqdeleteMins :: PSQueue.PSQ Int Int -> Int
psqdeleteMins = go 0
  where
    go !n t = case PSQueue.minView t of
      Nothing           -> n
      Just ((k PSQueue.:-> x), t') -> go (n + k + x) t'


-- Benchmarking our PSQ type
-------------------------------------------------------------------------------

ord_psqlookup :: [Int] -> OrdPSQ.PSQ Int Int () -> Int
ord_psqlookup xs m = foldl' (\n k -> maybe n fst (OrdPSQ.lookup k m)) 0 xs

ord_psqins :: [(Int, Int)] -> OrdPSQ.PSQ Int Int () -> OrdPSQ.PSQ Int Int ()
ord_psqins xs m0 = foldl' (\m (k, v) -> OrdPSQ.insert k v () m) m0 xs

ord_psqdeleteMins :: OrdPSQ.PSQ Int Int () -> Int
ord_psqdeleteMins = go 0
  where
    go !n t = case OrdPSQ.minView t of
      Nothing           -> n
      Just ((OrdPSQ.E k x _), t') -> go (n + k + x) t'


-- Benchmarking the psqueue from the fingertree-psqueue package
-------------------------------------------------------------------------------

fingerLookup :: [Int] -> FingerPSQ.PSQ Int Int -> Int
fingerLookup xs m = foldl' (\n k -> fromMaybe n (FingerPSQ.lookup k m)) 0 xs

fingerIns :: [(Int, Int)] -> FingerPSQ.PSQ Int Int -> FingerPSQ.PSQ Int Int
fingerIns xs m0 = foldl' (\m (k, v) -> fingerInsert k v m) m0 xs
  where
    fingerInsert :: (Ord k, Ord v) => k -> v -> FingerPSQ.PSQ k v -> FingerPSQ.PSQ k v
    fingerInsert k v m = FingerPSQ.alter (const $ Just v) k m

fingerDeleteMins :: FingerPSQ.PSQ Int Int -> Int
fingerDeleteMins = go 0
  where
    go !n t = case FingerPSQ.minView t of
      Nothing           -> n
      Just ((k FingerPSQ.:-> x), t') -> go (n + k + x) t'

