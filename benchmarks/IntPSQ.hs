{-# LANGUAGE BangPatterns #-}
module Main where

import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad.Trans (liftIO)
import Criterion.Config
import Criterion.Main
import Data.List (foldl')
import qualified Data.IntPSQ  as IntPSQ
import qualified Data.PSQueue as PSQ
import qualified Data.GHC_Events_PSQ as GHC_PSQ
import qualified Data.HashPSQ as HashPSQ
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map.Strict     as MS
import qualified Data.IntMap.Compact  as IMS
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

main = do

    let int_psq  = IntPSQ.fromList elems_with_unit :: IntPSQ.IntPSQ Int ()
        hms      = HMS.fromList elems :: HMS.HashMap Int Int
        ms       = MS.fromList elems :: MS.Map Int Int
        ims      = IMS.fromList elems :: IMS.IntMap Int

        psq      = PSQ.fromList psqelems :: PSQ.PSQ Int Int
        ghc_psq  = GHC_PSQ.fromList ghc_psqelems :: GHC_PSQ.PSQ ()
        hash_psq = HashPSQ.fromList elems_with_unit :: HashPSQ.HashPSQ Int Int ()

    defaultMainWith
        defaultConfig
        (liftIO $ do evaluate (rnf int_psq)
                     evaluate (rnf hms)
                     evaluate (rnf ms)
                     evaluate (rnf ims)
                     evaluate (rnf [ (x,y) | (x PSQ.:-> y) <- PSQ.toList psq])
                     evaluate (ghc_psq)
                     return ()
         )
        [ bench "HashMap.lookup" $ whnf (hashmap_lookup keys) hms
        , bench "Map.lookup" $ whnf (map_lookup keys) ms
        , bench "IntMap.lookup" $ whnf (intmap_lookup keys) ims

        , bench "HashMap.insert (fresh)" $ whnf (hashmap_ins elems) HMS.empty
        , bench "Map.insert (fresh)" $ whnf (map_ins elems) MS.empty
        , bench "IntMap.insert (fresh)" $ whnf (intmap_ins elems) IMS.empty

        , bench "HashPSQ.minView" $ whnf hash_psqdeleteMins hash_psq
        , bench "HashPSQ.lookup" $ whnf (hash_psqlookup keys) hash_psq
        , bench "HashPSQ.insert (fresh)" $ whnf (hash_psqins elems) HashPSQ.empty
        , bench "HashPSQ.insert (next fresh)" $ whnf (hash_psqins nextElems) hash_psq
        , bench "HashPSQ.insert (duplicates)" $ whnf (hash_psqins elems) hash_psq
        , bench "HashPSQ.insert (decreasing)" $ whnf (hash_psqins elemsDecreasing) hash_psq
        -- , bench "HashPSQ.fromList" $ whnf HashPSQ.fromList elems_with_unit

        , bench "GHC_Events_PSQ.minView" $ whnf ghc_psqdeleteMins ghc_psq
        , bench "GHC_Events_PSQ.lookup" $ whnf (ghc_psqlookup keys) ghc_psq
        , bench "GHC_Events_PSQ.insert (fresh)" $ whnf (ghc_psqins elems) GHC_PSQ.empty
        , bench "GHC_Events_PSQ.insert (next fresh)" $ whnf (ghc_psqins nextElems) ghc_psq
        , bench "GHC_Events_PSQ.insert (duplicates)" $ whnf (ghc_psqins elems) ghc_psq
        , bench "GHC_Events_PSQ.insert (decreasing)" $ whnf (ghc_psqins elemsDecreasing) ghc_psq
        -- , bench "GHC_Events_PSQ.fromList" $ whnf GHC_PSQ.fromList ghc_psqelems

        , bench "minView" $ whnf deleteMins int_psq
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

        , bench "PSQ.minView" $ whnf psqdeleteMins psq
        , bench "PSQ.lookup" $ whnf (psqlookup keys) psq
        , bench "PSQ.insert (fresh)" $ whnf (psqins elems) PSQ.empty
        , bench "PSQ.insert (duplicates)" $ whnf (psqins elems) psq
        , bench "PSQ.insert (decreasing)" $ whnf (psqins elemsDecreasing) psq
        , bench "PSQ.fromList" $ whnf PSQ.fromList psqelems
        -- , bench "union (with itself)" $ whnf (IntPSQ.union m) int_psq
        -- , bench "union (with itself shifted)" $ whnf (IntPSQ.union m2) int_psq
        -- , bench "insertWith empty" $ whnf (insWith elems) IntPSQ.empty
        -- , bench "insertWith update" $ whnf (insWith elems) int_psq
        -- , bench "insertWith' empty" $ whnf (insWith' elems) IntPSQ.empty
        -- , bench "insertWith' update" $ whnf (insWith' elems) int_psq
        -- , bench "insertWithKey empty" $ whnf (insWithKey elems) IntPSQ.empty
        -- , bench "insertWithKey update" $ whnf (insWithKey elems) int_psq
        -- , bench "insertWithKey' empty" $ whnf (insWithKey' elems) IntPSQ.empty
        -- , bench "insertWithKey' update" $ whnf (insWithKey' elems) int_psq
        -- , bench "insertLookupWithKey empty" $ whnf (insLookupWithKey elems) IntPSQ.empty
        -- , bench "insertLookupWithKey update" $ whnf (insLookupWithKey elems) int_psq
        -- , bench "map" $ whnf (IntPSQ.map (+ 1)) int_psq
        -- , bench "mapWithKey" $ whnf (IntPSQ.mapWithKey (+)) int_psq
        -- , bench "foldlWithKey" $ whnf (ins elems) int_psq
        -- , bench "foldlWithKey'" $ whnf (IntPSQ.foldlWithKey' sum 0) int_psq
        -- , bench "foldrWithKey" $ whnf (IntPSQ.foldrWithKey consPair []) int_psq
        -- , bench "delete" $ whnf (del keys) int_psq
        -- , bench "update" $ whnf (upd keys) int_psq
        -- , bench "updateLookupWithKey" $ whnf (upd' keys) int_psq
        -- , bench "alter"  $ whnf (alt keys) int_psq
        -- , bench "mapMaybe" $ whnf (IntPSQ.mapMaybe maybeDel) int_psq
        -- , bench "mapMaybeWithKey" $ whnf (IntPSQ.mapMaybeWithKey (const maybeDel)) int_psq
        -- , bench "fromAscList" $ whnf IntPSQ.fromAscList elems
        -- , bench "fromDistinctAscList" $ whnf IntPSQ.fromDistinctAscList elems
        ]
  where
    elems           = zip keys values
    elems_with_unit = [ (k,p,()) | (k,p) <- elems ]

    nextElems = [ (k + 2^12 + 1, p) | (k, p) <- elems ]

    elemsDecreasing = zip (reverse keys) (map negate values)
    psqelems        = map (uncurry (PSQ.:->)) elems
    ghc_psqelems    = map (\(k,p) -> GHC_PSQ.E k p ()) elems

    keys = [1..2^12]
    values = [1..2^12]
    sum k v1 v2 = k + v1 + v2
    consPair k v xs = (k, v) : xs

add3 :: Int -> Int -> Int -> Int
add3 x y z = x + y + z
{-# INLINE add3 #-}

hashmap_lookup :: [Int] -> HMS.HashMap Int Int -> Int
hashmap_lookup xs m = foldl' (\n k -> fromMaybe n (HMS.lookup k m)) 0 xs

map_lookup :: [Int] -> MS.Map Int Int -> Int
map_lookup xs m = foldl' (\n k -> fromMaybe n (MS.lookup k m)) 0 xs

intmap_lookup :: [Int] -> IMS.IntMap Int -> Int
intmap_lookup xs m = foldl' (\n k -> fromMaybe n (IMS.lookup k m)) 0 xs

lookup :: [Int] -> IntPSQ.IntPSQ Int () -> Int
lookup xs m = foldl' (\n k -> maybe n fst (IntPSQ.lookup k m)) 0 xs

hashmap_ins :: [(Int, Int)] -> HMS.HashMap Int Int -> HMS.HashMap Int Int
hashmap_ins xs m = foldl' (\m (k, v) -> HMS.insert k v m) m xs

map_ins :: [(Int, Int)] -> MS.Map Int Int -> MS.Map Int Int
map_ins xs m = foldl' (\m (k, v) -> MS.insert k v m) m xs

intmap_ins :: [(Int, Int)] -> IMS.IntMap Int -> IMS.IntMap Int
intmap_ins xs m = foldl' (\m (k, v) -> IMS.insert k v m) m xs

ins :: [(Int, Int)] -> IntPSQ.IntPSQ Int () -> IntPSQ.IntPSQ Int ()
ins xs m = foldl' (\m (k, v) -> IntPSQ.insert k v () m) m xs

ins2 :: [(Int, Int)] -> IntPSQ.IntPSQ Int () -> IntPSQ.IntPSQ Int ()
ins2 xs m = foldl' (\m (k, v) -> IntPSQ.insert2 k v () m) m xs

ins3 :: [(Int, Int)] -> IntPSQ.IntPSQ Int () -> IntPSQ.IntPSQ Int ()
ins3 xs m = foldl' (\m (k, v) -> IntPSQ.insert3 k v () m) m xs

deleteMins :: IntPSQ.IntPSQ Int () -> Int
deleteMins = go 0
  where
    go !n t = case IntPSQ.minViewWithKey t of
      Nothing           -> n
      Just ((k, p, _), t') -> go (n + k + p) t'

psqlookup :: [Int] -> PSQ.PSQ Int Int -> Int
psqlookup xs m = foldl' (\n k -> fromMaybe n (PSQ.lookup k m)) 0 xs

psqins :: [(Int, Int)] -> PSQ.PSQ Int Int -> PSQ.PSQ Int Int
psqins xs m = foldl' (\m (k, v) -> PSQ.insert k v m) m xs

psqdeleteMins :: PSQ.PSQ Int Int -> Int
psqdeleteMins = go 0
  where
    go !n t = case PSQ.minView t of
      Nothing           -> n
      Just ((k PSQ.:-> x), t') -> go (n + k + x) t'


ghc_psqlookup :: [Int] -> GHC_PSQ.PSQ () -> Int
ghc_psqlookup xs m = foldl' (\n k -> maybe n fst (GHC_PSQ.lookup k m)) 0 xs

ghc_psqins :: [(Int, Int)] -> GHC_PSQ.PSQ () -> GHC_PSQ.PSQ ()
ghc_psqins xs m = foldl' (\m (k, v) -> GHC_PSQ.insert k v () m) m xs

ghc_psqdeleteMins :: GHC_PSQ.PSQ () -> Int
ghc_psqdeleteMins = go 0
  where
    go !n t = case GHC_PSQ.minView t of
      Nothing           -> n
      Just ((GHC_PSQ.E k x _), t') -> go (n + k + x) t'

hash_psqlookup :: [Int] -> HashPSQ.HashPSQ Int Int () -> Int
hash_psqlookup xs m = foldl' (\n k -> maybe n fst (HashPSQ.lookup k m)) 0 xs

hash_psqins :: [(Int, Int)] -> HashPSQ.HashPSQ Int Int () -> HashPSQ.HashPSQ Int Int ()
hash_psqins xs m = foldl' (\m (k, v) -> HashPSQ.insert k v () m) m xs

hash_psqdeleteMins :: HashPSQ.HashPSQ Int Int () -> Int
hash_psqdeleteMins = go 0
  where
    go !n t = case HashPSQ.minView t of
      (Nothing       , _t') -> n
      (Just (k, x, _),  t') -> go (n + k + x) t'

{-
insWith :: [(Int, Int)] -> IntPSQ.IntPSQ Int () -> IntPSQ.IntPSQ Int ()
insWith xs m = foldl' (\m (k, v) -> IntPSQ.insertWith (+) k v m) m xs

insWithKey :: [(Int, Int)] -> IntPSQ.IntPSQ Int () -> IntPSQ.IntPSQ Int ()
insWithKey xs m = foldl' (\m (k, v) -> IntPSQ.insertWithKey add3 k v m) m xs

insWith' :: [(Int, Int)] -> IntPSQ.IntPSQ Int () -> IntPSQ.IntPSQ Int ()
insWith' xs m = foldl' (\m (k, v) -> IntPSQ.insertWith' (+) k v m) m xs

insWithKey' :: [(Int, Int)] -> IntPSQ.IntPSQ Int () -> IntPSQ.IntPSQ Int ()
insWithKey' xs m = foldl' (\m (k, v) -> IntPSQ.insertWithKey' add3 k v m) m xs

data PairS a b = PS !a !b

insLookupWithKey :: [(Int, Int)] -> IntPSQ.IntPSQ Int () -> (Int, IntPSQ.IntPSQ Int ())
insLookupWithKey xs m = let !(PS a b) = foldl' f (PS 0 m) xs in (a, b)
  where
    f (PS n m) (k, v) = let !(n', m') = IntPSQ.insertLookupWithKey add3 k v m
                        in PS (fromMaybe 0 n' + n) m'

del :: [Int] -> IntPSQ.IntPSQ Int () -> IntPSQ.IntPSQ Int ()
del xs m = foldl' (\m k -> IntPSQ.delete k m) m xs

upd :: [Int] -> IntPSQ.IntPSQ Int () -> IntPSQ.IntPSQ Int ()
upd xs m = foldl' (\m k -> IntPSQ.update Just k m) m xs

upd' :: [Int] -> IntPSQ.IntPSQ Int () -> IntPSQ.IntPSQ Int ()
upd' xs m = foldl' (\m k -> snd $ IntPSQ.updateLookupWithKey (\_ a -> Just a) k m) m xs

alt :: [Int] -> IntPSQ.IntPSQ Int () -> IntPSQ.IntPSQ Int ()
alt xs m = foldl' (\m k -> IntPSQ.alter id k m) m xs

maybeDel :: Int -> Maybe Int
maybeDel n | n `mod` 3 == 0 = Nothing
           | otherwise      = Just n
-}
