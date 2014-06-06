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
import qualified Data.GHC_Events_PSQ     as GHC_PSQ
import qualified Data.HashPSQ            as HashPSQ
import qualified Data.HashMap.Strict     as HashMap
import qualified Data.IntMap.Strict      as IntMap
import qualified Data.Map.Strict         as Map
import qualified Data.PSQueue            as PSQueue
import qualified Data.FingerTree.PSQueue as FingerPSQ

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

    elems           = zip keys values
    elems_with_unit = [ (k,p,()) | (k,p) <- elems ]
    nextElems       = [ (k + 2^(12 + 1 :: Int), p) | (k, p) <- elems ]

    elemsDecreasing = zip (reverse keys) values
    psqelems        = map (uncurry (PSQueue.:->)) elems
    fingerElems     = map (uncurry (FingerPSQ.:->)) elems
    ghc_psqelems    = map (\(k,p) -> GHC_PSQ.E k p ()) elems

    int_psq  = IntPSQ.fromList  elems_with_unit :: IntPSQ.IntPSQ Int ()
    hms      = HashMap.fromList elems           :: HashMap.HashMap Int Int
    ms       = Map.fromList     elems               :: Map.Map Int Int
    ims      = IntMap.fromList  elems            :: IntMap.IntMap Int

    psq      = PSQueue.fromList psqelems        :: PSQueue.PSQ Int Int
    ghc_psq  = GHC_PSQ.fromList ghc_psqelems    :: GHC_PSQ.PSQ ()
    hash_psq = HashPSQ.fromList elems_with_unit :: HashPSQ.HashPSQ Int Int ()
    finger   = FingerPSQ.fromList fingerElems        :: FingerPSQ.PSQ Int Int

    -- forcing the data
    forceData = do
        evaluate (rnf int_psq)
        evaluate (rnf hms)
        evaluate (rnf ms)
        evaluate (rnf ims)
        evaluate (rnf [ (x,y) | (x PSQueue.:-> y) <- PSQueue.toList psq])
        evaluate (rnf [ (x,y,z) | (GHC_PSQ.E x y z) <- GHC_PSQ.toList ghc_psq])
        return ()

    -- benchmarks
    benchmarks =
      [ bench_ghc_events_psq
      , bench_intpsq
      , bench_hashpsq
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

    bench_hashpsq = bgroup "HashPSQ"
      [ bench "minView" $ whnf hash_psqdeleteMins hash_psq
      , bench "lookup" $ whnf (hash_psqlookup keys) hash_psq
      , bench "insert (fresh)" $ whnf (hash_psqins elems) HashPSQ.empty
      , bench "insert (next fresh)" $ whnf (hash_psqins nextElems) hash_psq
      , bench "insert (duplicates)" $ whnf (hash_psqins elems) hash_psq
      , bench "insert (decreasing)" $ whnf (hash_psqins elemsDecreasing) hash_psq
      ]

    bench_ghc_events_psq = bgroup "GHC_Events_PSQ"
      [ bench "minView" $ whnf ghc_psqdeleteMins ghc_psq
      , bench "lookup" $ whnf (ghc_psqlookup keys) ghc_psq
      , bench "insert (fresh)" $ whnf (ghc_psqins elems) GHC_PSQ.empty
      , bench "insert (next fresh)" $ whnf (ghc_psqins nextElems) ghc_psq
      , bench "insert (duplicates)" $ whnf (ghc_psqins elems) ghc_psq
      , bench "insert (decreasing)" $ whnf (ghc_psqins elemsDecreasing) ghc_psq
      ]

    bench_intpsq = bgroup "IntPSQ"
      [ bench "minView" $ whnf deleteMins int_psq
      -- , bench "map (id)" $ whnf (IntPSQ.map id) int_psq
      -- , bench "map (negate)" $ whnf (IntPSQ.map negate) int_psq
      , bench "lookup" $ whnf (lookup keys) int_psq

      , bench "insert (fresh)" $ whnf (ins elems) IntPSQ.empty
      , bench "insert (next fresh)" $ whnf (ins nextElems) int_psq
      , bench "insert (duplicates)" $ whnf (ins elems) int_psq
      , bench "insert (decreasing)" $ whnf (ins elemsDecreasing) int_psq
      -- , bench "fromList" $ whnf IntPSQ.fromList elems_with_unit

      , bench "insert2 (fresh)" $ whnf (ins2 elems) IntPSQ.empty
      , bench "insert2 (duplicates)" $ whnf (ins2 elems) int_psq
      , bench "insert2 (decreasing)" $ whnf (ins2 elemsDecreasing) int_psq
      , bench "fromList2" $ whnf IntPSQ.fromList2 elems_with_unit

      , bench "insert3 (fresh)" $ whnf (ins3 elems) IntPSQ.empty
      , bench "insert3 (duplicates)" $ whnf (ins3 elems) int_psq
      , bench "insert3 (decreasing)" $ whnf (ins3 elemsDecreasing) int_psq
      , bench "fromList3" $ whnf IntPSQ.fromList3 elems_with_unit
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

lookup :: [Int] -> IntPSQ.IntPSQ Int () -> Int
lookup xs m = foldl' (\n k -> maybe n fst (IntPSQ.lookup k m)) 0 xs

hashmap_ins :: [(Int, Int)] -> HashMap.HashMap Int Int -> HashMap.HashMap Int Int
hashmap_ins xs m0 = foldl' (\m (k, v) -> HashMap.insert k v m) m0 xs

map_ins :: [(Int, Int)] -> Map.Map Int Int -> Map.Map Int Int
map_ins xs m0 = foldl' (\m (k, v) -> Map.insert k v m) m0 xs

intmap_ins :: [(Int, Int)] -> IntMap.IntMap Int -> IntMap.IntMap Int
intmap_ins xs m0 = foldl' (\m (k, v) -> IntMap.insert k v m) m0 xs


-- Benchmarking our IntPSQ type
-------------------------------------------------------------------------------

ins :: [(Int, Int)] -> IntPSQ.IntPSQ Int () -> IntPSQ.IntPSQ Int ()
ins xs m0 = foldl' (\m (k, v) -> IntPSQ.insert k v () m) m0 xs

ins2 :: [(Int, Int)] -> IntPSQ.IntPSQ Int () -> IntPSQ.IntPSQ Int ()
ins2 xs m0 = foldl' (\m (k, v) -> IntPSQ.insert2 k v () m) m0 xs

ins3 :: [(Int, Int)] -> IntPSQ.IntPSQ Int () -> IntPSQ.IntPSQ Int ()
ins3 xs m0 = foldl' (\m (k, v) -> IntPSQ.insert3 k v () m) m0 xs

deleteMins :: IntPSQ.IntPSQ Int () -> Int
deleteMins = go 0
  where
    go !n t = case IntPSQ.minViewWithKey t of
      Nothing           -> n
      Just ((k, p, _), t') -> go (n + k + p) t'

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


-- Benchmarking the psqueue from the GHC event manager
-------------------------------------------------------------------------------

ghc_psqlookup :: [Int] -> GHC_PSQ.PSQ () -> Int
ghc_psqlookup xs m = foldl' (\n k -> maybe n fst (GHC_PSQ.lookup k m)) 0 xs

ghc_psqins :: [(Int, Int)] -> GHC_PSQ.PSQ () -> GHC_PSQ.PSQ ()
ghc_psqins xs m0 = foldl' (\m (k, v) -> GHC_PSQ.insert k v () m) m0 xs

ghc_psqdeleteMins :: GHC_PSQ.PSQ () -> Int
ghc_psqdeleteMins = go 0
  where
    go !n t = case GHC_PSQ.minView t of
      Nothing           -> n
      Just ((GHC_PSQ.E k x _), t') -> go (n + k + x) t'


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


-- Benchmarking our HashPSQ type
-------------------------------------------------------------------------------

hash_psqlookup :: [Int] -> HashPSQ.HashPSQ Int Int () -> Int
hash_psqlookup xs m = foldl' (\n k -> maybe n fst (HashPSQ.lookup k m)) 0 xs

hash_psqins :: [(Int, Int)] -> HashPSQ.HashPSQ Int Int () -> HashPSQ.HashPSQ Int Int ()
hash_psqins xs m0 = foldl' (\m (k, v) -> HashPSQ.insert k v () m) m0 xs

hash_psqdeleteMins :: HashPSQ.HashPSQ Int Int () -> Int
hash_psqdeleteMins = go 0
  where
    go !n t = case HashPSQ.minView t of
      (Nothing       , _t') -> n
      (Just (k, x, _),  t') -> go (n + k + x) t'

