{-# LANGUAGE BangPatterns #-}
module Main where

import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad.Trans (liftIO)
import Criterion.Config
import Criterion.Main
import Data.List (foldl')
import qualified Data.IntMap.Compact as M
import qualified Data.PSQueue        as PSQ
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

main = do

    let m   = M.pfromList elems :: M.IntPSQ Int
        psq = PSQ.fromList psqelems :: PSQ.PSQ Int Int

    defaultMainWith
        defaultConfig
        (liftIO $ evaluate (rnf m) >> evaluate (rnf (PSQ.keys psq)))
        [ bench "minView" $ whnf deleteMins m
        , bench "map (id)" $ whnf (M.pmap id) m
        , bench "map (negate)" $ whnf (M.pmap negate) m
        , bench "lookup" $ whnf (lookup keys) m

        , bench "insert (fresh)" $ whnf (ins elems) M.pempty
        , bench "insert (duplicates)" $ whnf (ins elems) m
        , bench "insert (decreasing)" $ whnf (ins elemsDecreasing) m
        , bench "fromList" $ whnf M.pfromList elems

        , bench "insert2 (fresh)" $ whnf (ins2 elems) M.pempty
        , bench "insert2 (duplicates)" $ whnf (ins2 elems) m
        , bench "insert2 (decreasing)" $ whnf (ins2 elemsDecreasing) m
        , bench "fromList2" $ whnf M.pfromList2 elems

        , bench "insert3 (fresh)" $ whnf (ins3 elems) M.pempty
        , bench "insert3 (duplicates)" $ whnf (ins3 elems) m
        , bench "insert3 (decreasing)" $ whnf (ins3 elemsDecreasing) m
        , bench "fromList3" $ whnf M.pfromList3 elems

        , bench "PSQ.minView" $ whnf psqdeleteMins psq
        , bench "PSQ.lookup" $ whnf (psqlookup keys) psq
        , bench "PSQ.insert (fresh)" $ whnf (psqins elems) PSQ.empty
        , bench "PSQ.insert (duplicates)" $ whnf (psqins elems) psq
        , bench "PSQ.insert (decreasing)" $ whnf (psqins elemsDecreasing) psq
        , bench "PSQ.fromList" $ whnf PSQ.fromList psqelems
        -- , bench "union (with itself)" $ whnf (M.union m) m
        -- , bench "union (with itself shifted)" $ whnf (M.union m2) m
        -- , bench "insertWith empty" $ whnf (insWith elems) M.empty
        -- , bench "insertWith update" $ whnf (insWith elems) m
        -- , bench "insertWith' empty" $ whnf (insWith' elems) M.empty
        -- , bench "insertWith' update" $ whnf (insWith' elems) m
        -- , bench "insertWithKey empty" $ whnf (insWithKey elems) M.empty
        -- , bench "insertWithKey update" $ whnf (insWithKey elems) m
        -- , bench "insertWithKey' empty" $ whnf (insWithKey' elems) M.empty
        -- , bench "insertWithKey' update" $ whnf (insWithKey' elems) m
        -- , bench "insertLookupWithKey empty" $ whnf (insLookupWithKey elems) M.empty
        -- , bench "insertLookupWithKey update" $ whnf (insLookupWithKey elems) m
        -- , bench "map" $ whnf (M.map (+ 1)) m
        -- , bench "mapWithKey" $ whnf (M.mapWithKey (+)) m
        -- , bench "foldlWithKey" $ whnf (ins elems) m
        -- , bench "foldlWithKey'" $ whnf (M.foldlWithKey' sum 0) m
        -- , bench "foldrWithKey" $ whnf (M.foldrWithKey consPair []) m
        -- , bench "delete" $ whnf (del keys) m
        -- , bench "update" $ whnf (upd keys) m
        -- , bench "updateLookupWithKey" $ whnf (upd' keys) m
        -- , bench "alter"  $ whnf (alt keys) m
        -- , bench "mapMaybe" $ whnf (M.mapMaybe maybeDel) m
        -- , bench "mapMaybeWithKey" $ whnf (M.mapMaybeWithKey (const maybeDel)) m
        -- , bench "fromAscList" $ whnf M.fromAscList elems
        -- , bench "fromDistinctAscList" $ whnf M.fromDistinctAscList elems
        ]
  where
    elems = zip keys values

    elemsDecreasing = zip (reverse keys) (map negate values)
    psqelems        = map (uncurry (PSQ.:->)) elems

    keys = [1..2^12]
    values = [1..2^12]
    sum k v1 v2 = k + v1 + v2
    consPair k v xs = (k, v) : xs

add3 :: Int -> Int -> Int -> Int
add3 x y z = x + y + z
{-# INLINE add3 #-}

lookup :: [Int] -> M.IntPSQ Int -> Int
lookup xs m = foldl' (\n k -> fromMaybe n (M.plookup k m)) 0 xs

ins :: [(Int, Int)] -> M.IntPSQ Int -> M.IntPSQ Int
ins xs m = foldl' (\m (k, v) -> M.pinsert k v m) m xs

ins2 :: [(Int, Int)] -> M.IntPSQ Int -> M.IntPSQ Int
ins2 xs m = foldl' (\m (k, v) -> M.pinsert2 k v m) m xs

ins3 :: [(Int, Int)] -> M.IntPSQ Int -> M.IntPSQ Int
ins3 xs m = foldl' (\m (k, v) -> M.pinsert3 k v m) m xs

deleteMins :: M.IntPSQ Int -> Int
deleteMins = go 0
  where
    go !n t = case M.pminViewWithKey t of
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

{-
insWith :: [(Int, Int)] -> M.IntPSQ Int -> M.IntPSQ Int
insWith xs m = foldl' (\m (k, v) -> M.insertWith (+) k v m) m xs

insWithKey :: [(Int, Int)] -> M.IntPSQ Int -> M.IntPSQ Int
insWithKey xs m = foldl' (\m (k, v) -> M.insertWithKey add3 k v m) m xs

insWith' :: [(Int, Int)] -> M.IntPSQ Int -> M.IntPSQ Int
insWith' xs m = foldl' (\m (k, v) -> M.insertWith' (+) k v m) m xs

insWithKey' :: [(Int, Int)] -> M.IntPSQ Int -> M.IntPSQ Int
insWithKey' xs m = foldl' (\m (k, v) -> M.insertWithKey' add3 k v m) m xs

data PairS a b = PS !a !b

insLookupWithKey :: [(Int, Int)] -> M.IntPSQ Int -> (Int, M.IntPSQ Int)
insLookupWithKey xs m = let !(PS a b) = foldl' f (PS 0 m) xs in (a, b)
  where
    f (PS n m) (k, v) = let !(n', m') = M.insertLookupWithKey add3 k v m
                        in PS (fromMaybe 0 n' + n) m'

del :: [Int] -> M.IntPSQ Int -> M.IntPSQ Int
del xs m = foldl' (\m k -> M.delete k m) m xs

upd :: [Int] -> M.IntPSQ Int -> M.IntPSQ Int
upd xs m = foldl' (\m k -> M.update Just k m) m xs

upd' :: [Int] -> M.IntPSQ Int -> M.IntPSQ Int
upd' xs m = foldl' (\m k -> snd $ M.updateLookupWithKey (\_ a -> Just a) k m) m xs

alt :: [Int] -> M.IntPSQ Int -> M.IntPSQ Int
alt xs m = foldl' (\m k -> M.alter id k m) m xs

maybeDel :: Int -> Maybe Int
maybeDel n | n `mod` 3 == 0 = Nothing
           | otherwise      = Just n
-}
