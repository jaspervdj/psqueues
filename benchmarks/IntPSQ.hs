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
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

main = do

    let m       = IntPSQ.fromList elems :: IntPSQ.IntPSQ Int
        psq     = PSQ.fromList psqelems :: PSQ.PSQ Int Int
        ghc_psq = GHC_PSQ.fromList ghc_psqelems :: GHC_PSQ.PSQ ()

    defaultMainWith
        defaultConfig
        (liftIO $ do evaluate (rnf m)
                     evaluate (rnf [ (x,y) | (x PSQ.:-> y) <- PSQ.toList psq])
                     evaluate (ghc_psq)
                     return ()
         )

        [ bench "GHC_Events_PSQ.minView" $ whnf ghc_psqdeleteMins ghc_psq
        , bench "GHC_Events_PSQ.lookup" $ whnf (ghc_psqlookup keys) ghc_psq
        , bench "GHC_Events_PSQ.insert (fresh)" $ whnf (ghc_psqins elems) GHC_PSQ.empty
        , bench "GHC_Events_PSQ.insert (duplicates)" $ whnf (ghc_psqins elems) ghc_psq
        , bench "GHC_Events_PSQ.insert (decreasing)" $ whnf (ghc_psqins elemsDecreasing) ghc_psq
        , bench "GHC_Events_PSQ.fromList" $ whnf GHC_PSQ.fromList ghc_psqelems

        , bench "minView" $ whnf deleteMins m
        , bench "map (id)" $ whnf (IntPSQ.map id) m
        , bench "map (negate)" $ whnf (IntPSQ.map negate) m
        , bench "lookup" $ whnf (lookup keys) m

        , bench "insert (fresh)" $ whnf (ins elems) IntPSQ.empty
        , bench "insert (duplicates)" $ whnf (ins elems) m
        , bench "insert (decreasing)" $ whnf (ins elemsDecreasing) m
        , bench "fromList" $ whnf IntPSQ.fromList elems

        , bench "insert2 (fresh)" $ whnf (ins2 elems) IntPSQ.empty
        , bench "insert2 (duplicates)" $ whnf (ins2 elems) m
        , bench "insert2 (decreasing)" $ whnf (ins2 elemsDecreasing) m
        , bench "fromList2" $ whnf IntPSQ.fromList2 elems

        , bench "insert3 (fresh)" $ whnf (ins3 elems) IntPSQ.empty
        , bench "insert3 (duplicates)" $ whnf (ins3 elems) m
        , bench "insert3 (decreasing)" $ whnf (ins3 elemsDecreasing) m
        , bench "fromList3" $ whnf IntPSQ.fromList3 elems

        , bench "PSQ.minView" $ whnf psqdeleteMins psq
        , bench "PSQ.lookup" $ whnf (psqlookup keys) psq
        , bench "PSQ.insert (fresh)" $ whnf (psqins elems) PSQ.empty
        , bench "PSQ.insert (duplicates)" $ whnf (psqins elems) psq
        , bench "PSQ.insert (decreasing)" $ whnf (psqins elemsDecreasing) psq
        , bench "PSQ.fromList" $ whnf PSQ.fromList psqelems
        -- , bench "union (with itself)" $ whnf (IntPSQ.union m) m
        -- , bench "union (with itself shifted)" $ whnf (IntPSQ.union m2) m
        -- , bench "insertWith empty" $ whnf (insWith elems) IntPSQ.empty
        -- , bench "insertWith update" $ whnf (insWith elems) m
        -- , bench "insertWith' empty" $ whnf (insWith' elems) IntPSQ.empty
        -- , bench "insertWith' update" $ whnf (insWith' elems) m
        -- , bench "insertWithKey empty" $ whnf (insWithKey elems) IntPSQ.empty
        -- , bench "insertWithKey update" $ whnf (insWithKey elems) m
        -- , bench "insertWithKey' empty" $ whnf (insWithKey' elems) IntPSQ.empty
        -- , bench "insertWithKey' update" $ whnf (insWithKey' elems) m
        -- , bench "insertLookupWithKey empty" $ whnf (insLookupWithKey elems) IntPSQ.empty
        -- , bench "insertLookupWithKey update" $ whnf (insLookupWithKey elems) m
        -- , bench "map" $ whnf (IntPSQ.map (+ 1)) m
        -- , bench "mapWithKey" $ whnf (IntPSQ.mapWithKey (+)) m
        -- , bench "foldlWithKey" $ whnf (ins elems) m
        -- , bench "foldlWithKey'" $ whnf (IntPSQ.foldlWithKey' sum 0) m
        -- , bench "foldrWithKey" $ whnf (IntPSQ.foldrWithKey consPair []) m
        -- , bench "delete" $ whnf (del keys) m
        -- , bench "update" $ whnf (upd keys) m
        -- , bench "updateLookupWithKey" $ whnf (upd' keys) m
        -- , bench "alter"  $ whnf (alt keys) m
        -- , bench "mapMaybe" $ whnf (IntPSQ.mapMaybe maybeDel) m
        -- , bench "mapMaybeWithKey" $ whnf (IntPSQ.mapMaybeWithKey (const maybeDel)) m
        -- , bench "fromAscList" $ whnf IntPSQ.fromAscList elems
        -- , bench "fromDistinctAscList" $ whnf IntPSQ.fromDistinctAscList elems
        ]
  where
    elems = zip keys values

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

lookup :: [Int] -> IntPSQ.IntPSQ Int -> Int
lookup xs m = foldl' (\n k -> fromMaybe n (IntPSQ.lookup k m)) 0 xs

ins :: [(Int, Int)] -> IntPSQ.IntPSQ Int -> IntPSQ.IntPSQ Int
ins xs m = foldl' (\m (k, v) -> IntPSQ.insert k v m) m xs

ins2 :: [(Int, Int)] -> IntPSQ.IntPSQ Int -> IntPSQ.IntPSQ Int
ins2 xs m = foldl' (\m (k, v) -> IntPSQ.insert2 k v m) m xs

ins3 :: [(Int, Int)] -> IntPSQ.IntPSQ Int -> IntPSQ.IntPSQ Int
ins3 xs m = foldl' (\m (k, v) -> IntPSQ.insert3 k v m) m xs

deleteMins :: IntPSQ.IntPSQ Int -> Int
deleteMins = go 0
  where
    go !n t = case IntPSQ.minViewWithKey t of
      Nothing           -> n
      Just ((k, x), t') -> go (n + k + x) t'

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

{-
insWith :: [(Int, Int)] -> IntPSQ.IntPSQ Int -> IntPSQ.IntPSQ Int
insWith xs m = foldl' (\m (k, v) -> IntPSQ.insertWith (+) k v m) m xs

insWithKey :: [(Int, Int)] -> IntPSQ.IntPSQ Int -> IntPSQ.IntPSQ Int
insWithKey xs m = foldl' (\m (k, v) -> IntPSQ.insertWithKey add3 k v m) m xs

insWith' :: [(Int, Int)] -> IntPSQ.IntPSQ Int -> IntPSQ.IntPSQ Int
insWith' xs m = foldl' (\m (k, v) -> IntPSQ.insertWith' (+) k v m) m xs

insWithKey' :: [(Int, Int)] -> IntPSQ.IntPSQ Int -> IntPSQ.IntPSQ Int
insWithKey' xs m = foldl' (\m (k, v) -> IntPSQ.insertWithKey' add3 k v m) m xs

data PairS a b = PS !a !b

insLookupWithKey :: [(Int, Int)] -> IntPSQ.IntPSQ Int -> (Int, IntPSQ.IntPSQ Int)
insLookupWithKey xs m = let !(PS a b) = foldl' f (PS 0 m) xs in (a, b)
  where
    f (PS n m) (k, v) = let !(n', m') = IntPSQ.insertLookupWithKey add3 k v m
                        in PS (fromMaybe 0 n' + n) m'

del :: [Int] -> IntPSQ.IntPSQ Int -> IntPSQ.IntPSQ Int
del xs m = foldl' (\m k -> IntPSQ.delete k m) m xs

upd :: [Int] -> IntPSQ.IntPSQ Int -> IntPSQ.IntPSQ Int
upd xs m = foldl' (\m k -> IntPSQ.update Just k m) m xs

upd' :: [Int] -> IntPSQ.IntPSQ Int -> IntPSQ.IntPSQ Int
upd' xs m = foldl' (\m k -> snd $ IntPSQ.updateLookupWithKey (\_ a -> Just a) k m) m xs

alt :: [Int] -> IntPSQ.IntPSQ Int -> IntPSQ.IntPSQ Int
alt xs m = foldl' (\m k -> IntPSQ.alter id k m) m xs

maybeDel :: Int -> Maybe Int
maybeDel n | n `mod` 3 == 0 = Nothing
           | otherwise      = Just n
-}
