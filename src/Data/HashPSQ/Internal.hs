{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Data.HashPSQ.Internal
    ( -- * Type
      Bucket (..)
    , toBucket
    , HashPSQ (..)

      -- * Query
    , null
    , size
    , member
    , lookup
    , findMin

      -- * Construction
    , empty
    , singleton

      -- * Insertion
    , insert

      -- * Delete/update
    , delete
    , alter
    , alterMin

      -- * Lists
    , fromList
    , toList
    , keys

      -- * Views
    , deleteView
    , minView

      -- * Traversal
    , map
    , fold'
    ) where

import           Control.DeepSeq (NFData (..))
import           Data.Foldable   (Foldable (foldr))
import           Data.Hashable
import           Data.Maybe      (isJust)
import           Prelude         hiding (foldr, lookup, map, null)

import qualified Data.IntPSQ     as IntPSQ
import qualified Data.List       as List
import qualified Data.PSQ        as PSQ

------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

data Bucket k p v = B !k !v !(PSQ.PSQ k p v)
    deriving (Show)

{-# INLINABLE toBucket #-}
toBucket :: (Ord k, Ord p) => PSQ.PSQ k p v -> Maybe (p, Bucket k p v)
toBucket opsq = case PSQ.minView opsq of
    Nothing               -> Nothing
    Just (k, p, x, opsq') -> Just (p, B k x opsq')

instance (NFData k, NFData p, NFData v) => NFData (Bucket k p v) where
    rnf (B k v x) = rnf k `seq` rnf v `seq` rnf x

newtype HashPSQ k p v = HashPSQ (IntPSQ.IntPSQ p (Bucket k p v))
    deriving (NFData, Show)

instance (Eq k, Eq p, Eq v, Hashable k, Ord k, Ord p) =>
            Eq (HashPSQ k p v) where
    x == y = case (minView x, minView y) of
        (Nothing              , Nothing                ) -> True
        (Just (xk, xp, xv, x'), (Just (yk, yp, yv, y'))) ->
            xk == yk && xp == yp && xv == yv && x' == y'
        (Just _               , Nothing                ) -> False
        (Nothing              , Just _                 ) -> False

instance Foldable (HashPSQ k p) where
    foldr f z0 (HashPSQ ipsq) =
        foldr f' z0 ipsq
      where
        f' (B _ x opsq) z = f x (foldr f z opsq)

instance Functor (HashPSQ k p) where
    fmap f = map (\_ _ v -> f v)


------------------------------------------------------------------------------
-- Query
------------------------------------------------------------------------------

{-# INLINABLE null #-}
null :: HashPSQ k p v -> Bool
null (HashPSQ ipsq) = IntPSQ.null ipsq

{-# INLINABLE size #-}
size :: (Hashable k, Ord p) => HashPSQ k p v -> Int
size (HashPSQ ipsq) = IntPSQ.fold'
    (\_ _ (B _ _ opsq) acc -> 1 + PSQ.size opsq + acc)
    0
    ipsq

{-# INLINABLE member #-}
member :: (Hashable k, Ord k, Ord p) => k -> HashPSQ k p v -> Bool
member k = isJust . lookup k

{-# INLINABLE lookup #-}
lookup :: (Ord k, Hashable k, Ord p) => k -> HashPSQ k p v -> Maybe (p, v)
lookup k (HashPSQ ipsq) = do
    (p0, B k0 v0 os) <- IntPSQ.lookup (hash k) ipsq
    if k0 == k
        then return (p0, v0)
        else PSQ.lookup k os

findMin :: (Hashable k, Ord k, Ord p) => HashPSQ k p v -> Maybe (k, p, v)
findMin t = case minView t of
    Nothing           -> Nothing
    Just (k, p, v, _) -> Just (k, p, v)


--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

empty :: HashPSQ k p v
empty = HashPSQ IntPSQ.empty

-- | /O(1)/ Build a queue with one element.
singleton :: (Hashable k, Ord k, Ord p) => k -> p -> v -> HashPSQ k p v
singleton k p v = insert k p v empty


--------------------------------------------------------------------------------
-- Insertion
--------------------------------------------------------------------------------

{-# INLINABLE insert #-}
insert :: (Ord k, Hashable k, Ord p)
       => k -> p -> v -> HashPSQ k p v -> HashPSQ k p v
insert k p v (HashPSQ ipsq) =
    HashPSQ (snd (IntPSQ.alter (\x -> ((), ins x)) (hash k) ipsq))
  where
    ins Nothing           = Just (p,  B k  v  (PSQ.empty             ))
    ins (Just (p', B k' v' os))
        | k' == k         =
            -- Tricky: p might have less priority than an item in 'os'.
            toBucket (PSQ.insert k p v os)
        | p' <= p         =
            Just (p', B k' v' (PSQ.insert k  p  v  os))
        | PSQ.member k os =
            -- This is a bit tricky: k might already be present in 'os' and we
            -- don't want to end up with duplicate keys.
            Just (p,  B k  v  (PSQ.insert k' p' v' (PSQ.delete k os)))
        | otherwise       =
            Just (p , B k  v  (PSQ.insert k' p' v' os))




--------------------------------------------------------------------------------
-- Delete/update
--------------------------------------------------------------------------------

{-# INLINE delete #-}
delete
    :: (Hashable k, Ord k, Ord p) => k -> HashPSQ k p v -> HashPSQ k p v
delete k t = case deleteView k t of
    Nothing         -> t
    Just (_, _, t') -> t'

{-# INLINE alter #-}
alter :: (Hashable k, Ord k, Ord p)
      => (Maybe (p, v) -> (b, Maybe (p, v)))
      -> k -> HashPSQ k p v -> (b, HashPSQ k p v)
alter f k t0 =
    -- TODO (jaspervdj): Both 'deleteView' and 'insert' act on the same bucket
    -- so there should be a much faster way to do this.
    let (t, mbX) = case deleteView k t0 of
                            Nothing          -> (t0, Nothing)
                            Just (p, x, t0') -> (t0', Just (p, x))
    in case f mbX of
        (b, mbX') ->
            (b, maybe t (\(p, x) -> insert k p x t) mbX')

{-# INLINABLE alterMin #-}
alterMin
    :: (Hashable k, Ord k, Ord p)
     => (Maybe (k, p, v) -> (b, Maybe (k, p, v)))
     -> HashPSQ k p v
     -> (b, HashPSQ k p v)
alterMin f t0 =
    let (t, mbX) = case minView t0 of
                    Nothing             -> (t0, Nothing)
                    Just (k, p, x, t0') -> (t0', Just (k, p, x))
    in case f mbX of
        (b, mbX') ->
            (b, maybe t (\(k, p, x) -> insert k p x t) mbX')


--------------------------------------------------------------------------------
-- Lists
--------------------------------------------------------------------------------

{-# INLINABLE fromList #-}
fromList :: (Hashable k, Ord k, Ord p) => [(k, p, v)] -> HashPSQ k p v
fromList = List.foldl' (\psq (k, p, x) -> insert k p x psq) empty


{-# INLINABLE toList #-}
toList :: (Hashable k, Ord k, Ord p) => HashPSQ k p v -> [(k, p, v)]
toList (HashPSQ ipsq) =
    [ (k', p', x')
    | (_, p, (B k x opsq)) <- IntPSQ.toList ipsq
    , (k', p', x')         <- (k, p, x) : PSQ.toList opsq
    ]

{-# INLINABLE keys #-}
keys :: (Hashable k, Ord k, Ord p) => HashPSQ k p v -> [k]
keys t = [k | (k, _, _) <- toList t]


--------------------------------------------------------------------------------
-- Views
--------------------------------------------------------------------------------

{-# INLINABLE deleteView #-}
deleteView
    :: forall k p v. (Hashable k, Ord k, Ord p)
    => k -> HashPSQ k p v -> Maybe (p, v, HashPSQ k p v)
deleteView k (HashPSQ ipsq) = case IntPSQ.alter f (hash k) ipsq of
    (Nothing,     _    ) -> Nothing
    (Just (p, x), ipsq') -> Just (p, x, HashPSQ ipsq')
  where
    f :: Maybe (p, Bucket k p v) -> (Maybe (p, v), Maybe (p, Bucket k p v))
    f Nothing       = (Nothing, Nothing)
    f (Just (p, B bk bx opsq))
        | k == bk   = case PSQ.minView opsq of
            Nothing                  -> (Just (p, bx), Nothing)
            Just (k', p', x', opsq') -> (Just (p, bx), Just (p', B k' x' opsq'))
        | otherwise = case PSQ.deleteView k opsq of
            Nothing              -> (Nothing,       Nothing)
            Just (p', x', opsq') -> (Just (p', x'), Just (p, B bk bx opsq'))

{-# INLINABLE minView #-}
minView
    :: (Hashable k, Ord k, Ord p)
    => HashPSQ k p v -> Maybe (k, p, v, HashPSQ k p v)
minView (HashPSQ ipsq ) =
    case IntPSQ.alterMin f ipsq of
        (Nothing       , _    ) -> Nothing
        (Just (k, p, x), ipsq') -> Just (k, p, x, HashPSQ ipsq')
  where
    f Nothing                 = (Nothing, Nothing)
    f (Just (h, p, B k x os)) = case PSQ.minView os of
        Nothing                ->
            (Just (k, p, x), Nothing)
        Just (k', p', x', os') ->
            (Just (k, p, x), Just (h, p', B k' x' os'))


--------------------------------------------------------------------------------
-- Traversals
--------------------------------------------------------------------------------

{-# INLINABLE map #-}
map :: (k -> p -> v -> w) -> HashPSQ k p v -> HashPSQ k p w
map f (HashPSQ ipsq) = HashPSQ (IntPSQ.map (\_ p v -> mapBucket p v) ipsq)
  where
    mapBucket p (B k v opsq) = B k (f k p v) (PSQ.map f opsq)

{-# INLINABLE fold' #-}
fold' :: (k -> p -> v -> a -> a) -> a -> HashPSQ k p v -> a
fold' f acc0 (HashPSQ ipsq) = IntPSQ.fold' goBucket acc0 ipsq
  where
    goBucket _ p (B k v opsq) acc =
        let !acc1 = f k p v acc
            !acc2 = PSQ.fold' f acc1 opsq
        in acc2
