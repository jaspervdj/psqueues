{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.WIntPSQ
    ( WIntPSQ

      -- * Query
    , size
    , null
    , lookup
    , member
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

import           Control.DeepSeq (NFData(rnf))

import           Data.Foldable (Foldable (foldr))
import           Data.List (foldl')

import           Prelude hiding (lookup, map, filter, foldr, foldl, null)

import qualified Data.IntPSQ.Internal as O


------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

type Key  = Int
type Size = Int

-- | Wrapped version of IntPSQ which carries the size and the maximum seen priority
data WIntPSQ p v
    = WIntPSQ           !p              -- Maximum priority seen
        {- #UNPACK # -} !Size           -- Size of the PSQ
                        !(O.IntPSQ p v) -- Root node
    | Empty
    deriving (Show)

------------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------------

instance (NFData p, NFData v) => NFData (WIntPSQ p v) where
    rnf (WIntPSQ p s psq)   = rnf p `seq` rnf s `seq` rnf psq
    rnf Empty                 = ()


instance (Ord p, Eq v) => Eq (WIntPSQ p v) where
    x == y = case (minView x, minView y) of
        (Nothing              , Nothing                ) -> True
        (Just (xk, xp, xv, x'), (Just (yk, yp, yv, y'))) ->
            xk == yk && xp == yp && xv == yv && x' == y'
        (Just _               , Nothing                ) -> False
        (Nothing              , Just _                 ) -> False

instance Foldable (WIntPSQ p) where
    foldr _ z Empty             = z
    foldr f z (WIntPSQ _ _ psq) = foldr f z psq

instance Functor (WIntPSQ p) where
    fmap f = map (\_ _ v -> f v)

------------------------------------------------------------------------------
-- Query
------------------------------------------------------------------------------

null :: WIntPSQ p v -> Bool
null (Empty)           = True
null (WIntPSQ _ _ psq) = O.null psq

size :: WIntPSQ p v -> Int
size Empty              = 0
size (WIntPSQ _ s _)    = s

lookup :: Key -> WIntPSQ p v -> Maybe (p, v)
lookup _ Empty             = Nothing
lookup k (WIntPSQ _ _ psq) = O.lookup k psq

member :: Key -> WIntPSQ p v -> Bool
member _ Empty             = False
member k (WIntPSQ _ _ psq) = O.member k psq

findMin :: Ord p => WIntPSQ p v -> Maybe (Int, p, v)
findMin Empty             = Nothing
findMin (WIntPSQ _ _ psq) = O.findMin psq

------------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------------

empty :: WIntPSQ p v
empty = Empty

singleton :: Ord p => Key -> p -> v -> WIntPSQ p v
singleton k p v = WIntPSQ p 1 $ O.Tip k p v

------------------------------------------------------------------------------
-- Insertion
------------------------------------------------------------------------------

{-# INLINE insert #-}
insert :: Ord p => Key -> p -> v -> WIntPSQ p v -> WIntPSQ p v
insert k p x Empty = WIntPSQ p 1 $ O.Tip k p x
insert k p x (WIntPSQ maxPrio psqSize psq)
    | p > maxPrio = wrapInsertView p       psqSize $ O.insertLargerThanMaxPrioView k p x psq
    | otherwise   = wrapInsertView maxPrio psqSize $ O.insertView k p x psq

wrapInsertView :: Ord p => p -> Size -> (O.IntPSQ p v, Maybe (p, v)) -> WIntPSQ p v
wrapInsertView maxPrio psqSize !(psq, Nothing) = WIntPSQ maxPrio (psqSize+1) psq
wrapInsertView maxPrio psqSize !(psq, Just _)  = WIntPSQ maxPrio psqSize     psq

------------------------------------------------------------------------------
-- Deletion
------------------------------------------------------------------------------

{-# INLINE delete #-}
delete :: Ord p => Key -> WIntPSQ p v -> WIntPSQ p v
delete _ Empty = Empty
delete k (WIntPSQ maxPrio psqSize psq) =
    case O.deleteView k psq of
      Nothing        -> WIntPSQ maxPrio psqSize psq
      Just (_, _, t) -> if O.null t then Empty else WIntPSQ maxPrio (psqSize-1) t

-- TODO: alter, alterMin. These are more involved to wrap as size may change.
alter
    :: Ord p
    => (Maybe (p, v) -> (b, Maybe (p, v)))
    -> Key
    -> WIntPSQ p v
    -> (b, WIntPSQ p v)
alter = undefined

alterMin :: Ord p
         => (Maybe (Key, p, v) -> (b, Maybe (Key, p, v)))
         -> WIntPSQ p v
         -> (b, WIntPSQ p v)
alterMin = undefined

------------------------------------------------------------------------------
-- Lists
------------------------------------------------------------------------------

{-# INLINE fromList #-}
fromList :: Ord p => [(Key, p, v)] -> WIntPSQ p v
fromList = foldl' (\im (k, p, x) -> insert k p x im) empty

{-# INLINE toList #-}
toList :: WIntPSQ p v -> [(Int, p, v)]
toList Empty = []
toList (WIntPSQ _ _ psq) = O.toList psq


{-# INLINE keys #-}
keys :: WIntPSQ p v  -> [Int]
keys Empty = []
keys (WIntPSQ _ _ psq) = O.keys psq

------------------------------------------------------------------------------
-- Views
------------------------------------------------------------------------------

{-# INLINE minView #-}
minView :: Ord p => WIntPSQ p v -> Maybe (Key, p, v, WIntPSQ p v)
minView Empty = Nothing
minView (WIntPSQ maxPrio psqSize psq) =
    case O.minView psq of
      Just (k, p, x, psq') -> Just (k, p, x, WIntPSQ maxPrio (psqSize-1) psq')
      Nothing                -> Nothing

{-# INLINE deleteView #-}
deleteView :: Ord p => Key -> WIntPSQ p v -> Maybe (p, v, WIntPSQ p v)
deleteView _ Empty =
    Nothing
deleteView k (WIntPSQ maxPrio psqSize psq) =
    case O.deleteView k psq of
      Nothing           -> Nothing
      Just (p, v, psq') -> Just (p, v, WIntPSQ maxPrio psqSize psq')

------------------------------------------------------------------------------
-- Traversal
------------------------------------------------------------------------------

{-# INLINE map #-}
map :: (Int -> p -> v -> w) -> WIntPSQ p v -> WIntPSQ p w
map _ Empty = Empty
map f (WIntPSQ maxPrio psqSize psq) = WIntPSQ maxPrio psqSize $ O.map f psq

{-# INLINABLE fold' #-}
fold' :: (Int -> p -> v -> a -> a) -> a -> WIntPSQ p v -> a
fold' _ acc Empty = acc
fold' f acc (WIntPSQ _ _ psq) = O.fold' f acc psq
