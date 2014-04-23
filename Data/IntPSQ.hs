{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.IntPSQ
    ( IntPSQ(..)
    , map
    , fromList
    , lookup
    , insert
    , insert2
    , fromList2
    , insert3
    , fromList3
    , empty
    , minViewWithKey
    ) where

import           Control.Applicative (Applicative(pure, (<*>)), (<$>))
import           Control.DeepSeq (NFData(rnf))
import           Control.Monad (liftM)

import           Data.BitUtil
import           Data.Bits
-- import qualified Data.Foldable as Foldable
import           Data.List (foldl')
import           Data.Maybe (fromMaybe, isJust)
-- import           Data.Monoid (Monoid(..))
import           Data.Traversable (Traversable(traverse))
import           Data.Typeable
import           Data.Word (Word)

import           Prelude hiding (lookup, map, filter, foldr, foldl, null)


-- TODO (SM): get rid of bang patterns

{-
-- Use macros to define strictness of functions.
-- STRICT_x_OF_y denotes an y-ary function strict in the x-th parameter.
-- We do not use BangPatterns, because they are not in any standard and we
-- want the compilers to be compiled by as many compilers as possible.
#define STRICT_1_OF_2(fn) fn arg _ | arg `seq` False = undefined
-}


------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

-- A "Nat" is a natural machine word (an unsigned Int)
type Nat = Word

type Key = Int

-- | We store masks as the index of the bit that determines the branching.
type Mask = Int


data IntPSQ a
    = Bin {-# UNPACK #-} !Key !a {-# UNPACK #-} !Mask !(IntPSQ a) !(IntPSQ a)
    | Tip {-# UNPACK #-} !Key !a
    | Nil
    deriving (Eq, Show)


-- instances
------------

instance NFData a => NFData (IntPSQ a) where
    rnf t = t `seq` ()


-- bit twiddling
----------------

{-# INLINE natFromInt #-}
natFromInt :: Key -> Nat
natFromInt = fromIntegral

{-# INLINE intFromNat #-}
intFromNat :: Nat -> Key
intFromNat = fromIntegral

{-# INLINE zero #-}
zero :: Key -> Mask -> Bool
zero i m
  = (natFromInt i) .&. (natFromInt m) == 0

{-# INLINE nomatch #-}
nomatch :: Key -> Key -> Mask -> Bool
nomatch k1 k2 m =
    natFromInt k1 .&. m' /= natFromInt k2 .&. m'
  where
    m' = maskW (natFromInt m)

{-# INLINE maskW #-}
maskW :: Nat -> Nat
maskW m = complement (m-1) `xor` m

{-# INLINE branchMask #-}
branchMask :: Key -> Key -> Mask
branchMask k1 k2 =
    intFromNat (highestBitMask (natFromInt k1 `xor` natFromInt k2))


------------------------------------------------------------------------------
-- Queries
------------------------------------------------------------------------------

null :: IntPSQ a -> Bool
null Nil = True
null _   = False

lookup :: Key -> IntPSQ a -> Maybe a
lookup k t = case t of
    Nil                -> Nothing

    Tip k' x'
      | k == k'        -> Just x'
      | otherwise      -> Nothing

    Bin k' x' m l r
      | nomatch k k' m -> Nothing
      | k == k'        -> Just x'
      | zero k m       -> lookup k l
      | otherwise      -> lookup k r


------------------------------------------------------------------------------
--- Construction
------------------------------------------------------------------------------


empty :: IntPSQ a
empty = Nil

-- Insertion
------------

-- | This variant of insert has the most consistent performance. It does at
-- most two root-to-leaf traversals, which are reallocating the nodes on their
-- path.
{-# INLINE insert #-}
insert :: Ord a => Key -> a -> IntPSQ a -> IntPSQ a
insert k x t0 =
    -- case deleteView k t0 of
    --   (t, _mbX) -> insertNew k x t
    insertNew k x (delete k t0)
    {-
    case lookup k t of
      Nothing   -> insertNew k x t
      Just _mbX -> insertNew k x (delete k t)
    -}

{-# INLINE alter #-}
alter :: Ord a => (Maybe a -> (Maybe a, b)) -> Key -> IntPSQ a -> (IntPSQ a, b)
alter mkNextX k t0 =
    case deleteView k t0 of
      (t, mbX) ->
        case mkNextX mbX of
          (Nothing, result) -> (t,               result)
          (Just x,  result) -> (insertNew k x t, result)


-- | Internal function to insert a key that is *not* present in the priority
-- queue.
{-# INLINABLE insertNew #-}
insertNew :: Ord a => Key -> a -> IntPSQ a -> IntPSQ a
insertNew k x t = case t of
  Nil       -> Tip k x

  Tip k' x'
    | (x, k) < (x', k') -> link k  x  k' t         Nil
    | otherwise         -> link k' x' k  (Tip k x) Nil

  Bin k' x' m l r
    | nomatch k k' m ->
        if (x, k) < (x', k')
          then link k  x  k' t         Nil
          else link k' x' k  (Tip k x) (merge m l r)

    | otherwise ->
        if (x, k) < (x', k')
          then
            if zero k' m
              then Bin k  x  m (insertNew k' x' l) r
              else Bin k  x  m l                   (insertNew k' x' r)
          else
            if zero k m
              then Bin k' x' m (insertNew k  x  l) r
              else Bin k' x' m l                   (insertNew k  x  r)




-- | A more clever variant of insert that first looks up the key and then
-- re-establishes the min-heap property in a bottom-up fashion.
--
-- NOTE (SM): the performacne of this function is bad if there are many
-- priority decrements of keys that are deep down in the map. I think it might
-- even have a quadratic worst-case performance because of the repeated calls
-- to 'merge'.
{-# INLINABLE insert2 #-}
insert2 :: Ord a => Key -> a -> IntPSQ a -> IntPSQ a
insert2 k x =
    go
  where
    go t = case t of
      Nil -> Tip k x

      Tip k' x'
        | k == k'           -> Tip k x
        | (x, k) < (x', k') -> link k  x  k' t         Nil
        | otherwise         -> link k' x' k  (Tip k x) Nil

      Bin k' x' m l r
        | nomatch k k' m ->
            if (x, k) < (x', k')
              then link k  x  k' t         Nil
              else link k' x' k  (Tip k x) (merge m l r)

        | k == k' ->
            case compare x x' of
              LT -> Bin k x m l r
              EQ -> t
              GT -> insertNew k x (merge m l r)
        | zero k m  -> binBubbleL k' x' m (go l) r
        | otherwise -> binBubbleR k' x' m l      (go r)

-- | A smart constructor for a 'Bin' node whose left subtree's root could have
-- a smaller priority and therefore needs to be bubbled up.
{-# INLINE binBubbleL #-}
binBubbleL :: Ord a => Key -> a -> Mask -> IntPSQ a -> IntPSQ a -> IntPSQ a
binBubbleL k x m l r = case l of
    Nil                   -> Bin k  x  m Nil                r
    Tip lk lx
      | (x, k) < (lx, lk) -> Bin k  x  m l                  r
      | zero k m          -> Bin lk lx m (Tip k x)          r
      | otherwise         -> Bin lk lx m Nil                (insertNew k x r)

    Bin lk lx lm ll lr
      | (x, k) < (lx, lk) -> Bin k  x  m l                  r
      | zero k m          -> Bin lk lx m (Bin k x lm ll lr) r
      | otherwise         -> Bin lk lx m (merge lm ll lr)  (insertNew k x r)

-- | A smart constructor for a 'Bin' node whose right subtree's root could
-- have a smaller priority and therefore needs to be bubbled up.
{-# INLINE binBubbleR #-}
binBubbleR :: Ord a => Key -> a -> Mask -> IntPSQ a -> IntPSQ a -> IntPSQ a
binBubbleR k x m l r = case r of
    Nil                   -> Bin k  x  m l Nil
    Tip rk rx
      | (x, k) < (rx, rk) -> Bin k  x  m l r
      | zero k m          -> Bin rk rx m (insertNew k x l) Nil
      | otherwise         -> Bin rk rx m l                 (Tip k x)

    Bin rk rx rm rl rr
      | (x, k) < (rx, rk) -> Bin k  x  m l r
      | zero k m          -> Bin rk rx m (insertNew k x l) (merge rm rl rr)
                             -- NOTE that this case can be quite expensive, as
                             -- we might end up merging the same case multiple
                             -- times.
      | otherwise         -> Bin rk rx m l                 (Bin k x rm rl rr)

-- | A variant of insert that fuses the delete pass and the insertNew pass and
-- does not need to re-establish the min-heap property in a bottom-up fashion.
--
-- NOTE (SM) surprisingly, it is slower in benchmarks, which might be cause it
-- is buggy, or because there's some bad Core being generated.
{-# INLINABLE insert3 #-}
insert3 :: Ord a => Key -> a -> IntPSQ a -> IntPSQ a
insert3 k x t = case t of
    Nil -> Tip k x

    Tip k' x' ->
      case compare k k' of
        EQ -> Tip k' x
        LT -> if x <= x'
                then link k  x  k' t         Nil
                else link k' x' k  (Tip k x) Nil
        GT -> if x < x'
                then link k  x  k' t         Nil
                else link k' x' k  (Tip k x) Nil

    Bin k' x' m l r
      | nomatch k k' m ->
          if (x, k) < (x', k')
            then link k  x  k' t         Nil
            else link k' x' k  (Tip k x) (merge m l r)

      | k == k' ->
          case compare x x' of
            LT -> Bin k' x m l r
            EQ -> t
            GT -> insertNew k x (merge m l r)

      | (x, k) < (x', k') ->
          case (zero k m, zero k' m) of
            (False, False) -> Bin k x m                            l   (insertNew k' x' (delete k r))
            (False, True ) -> Bin k x m (insertNew k' x'           l )                  (delete k r)
            (True,  False) -> Bin k x m                  (delete k l)  (insertNew k' x'           r )
            (True,  True ) -> Bin k x m (insertNew k' x' (delete k l))                            r

      | otherwise ->
          if zero k m
            then Bin k' x' m (insert k x l) r
            else Bin k' x' m l              (insert k x r)




link :: Key -> a -> Key -> IntPSQ a -> IntPSQ a -> IntPSQ a
link k x k' k't otherTree
  | zero m k' = Bin k x m k't       otherTree
  | otherwise = Bin k x m otherTree k't
  where
    m = branchMask k k'

-- | Internal function that merges two *disjoint* 'IntPSQ's that share the
-- same prefix mask.
{-# INLINABLE merge #-}
merge :: Ord a => Mask -> IntPSQ a -> IntPSQ a -> IntPSQ a
merge m l r = case l of
    Nil -> r

    Tip lk lx ->
      case r of
        Nil                     -> l
        Tip rk rx
          | (lx, lk) < (rx, rk) -> Bin lk lx m Nil r
          | otherwise           -> Bin rk rx m l   Nil
        Bin rk rx rm rl rr
          | (lx, lk) < (rx, rk) -> Bin lk lx m Nil r
          | otherwise           -> Bin rk rx m l   (merge rm rl rr)

    Bin lk lx lm ll lr ->
      case r of
        Nil                     -> l
        Tip rk rx
          | (lx, lk) < (rx, rk) -> Bin lk lx m (merge lm ll lr) r
          | otherwise           -> Bin rk rx m l                Nil
        Bin rk rx rm rl rr
          | (lx, lk) < (rx, rk) -> Bin lk lx m (merge lm ll lr) r
          | otherwise           -> Bin rk rx m l                (merge rm rl rr)



-- FromList
-----------

{-# INLINABLE fromList #-}
fromList :: Ord a => [(Key, a)] -> IntPSQ a
fromList = foldl' (\im (k, x) -> insert k x im) empty

{-# INLINABLE fromList2 #-}
fromList2 :: Ord a => [(Key, a)] -> IntPSQ a
fromList2 = foldl' (\im (k, x) -> insert2 k x im) empty

{-# INLINABLE fromList3 #-}
fromList3 :: Ord a => [(Key, a)] -> IntPSQ a
fromList3 = foldl' (\im (k, x) -> insert3 k x im) empty


------------------------------------------------------------------------------
-- Modification
------------------------------------------------------------------------------


{-# INLINABLE map #-}
map :: Ord b => (a -> b) -> IntPSQ a -> IntPSQ b
map f =
    go
  where
    go t = case t of
        Nil           -> Nil
        Tip k x       -> Tip k (f x)
        Bin k x m l r -> insertNew k (f x) (merge m (go l) (go r))
                         -- TODO (SM): try to avoid the merge by pulling one
                         -- level up.

------------------------------------------------------------------------------
-- Destruction
------------------------------------------------------------------------------


{-# INLINE minViewWithKey #-}
minViewWithKey :: Ord a => IntPSQ a -> Maybe ((Key, a), IntPSQ a)
minViewWithKey t = case t of
    Nil           -> Nothing
    Tip k x       -> Just ((k, x), Nil)
    Bin k x m l r -> Just ((k, x), merge m l r)


-- | Smart constructor for a 'Bin' node whose left subtree could have become
-- 'Nil'.
{-# INLINE binShrinkL #-}
binShrinkL :: Key -> a -> Mask -> IntPSQ a -> IntPSQ a -> IntPSQ a
binShrinkL k x m Nil r = case r of Nil -> Tip k x; _ -> Bin k x m Nil r
binShrinkL k x m l   r = Bin k x m l r

-- | Smart constructor for a 'Bin' node whose right subtree could have become
-- 'Nil'.
{-# INLINE binShrinkR #-}
binShrinkR :: Key -> a -> Mask -> IntPSQ a -> IntPSQ a -> IntPSQ a
binShrinkR k x m l Nil = case l of Nil -> Tip k x; _ -> Bin k x m l Nil
binShrinkR k x m l r   = Bin k x m l r


-- TODO (SM): verify that it is really worth do do deletion and lookup at the
-- same time.
{-# INLINABLE deleteView #-}
deleteView :: Ord a => Key -> IntPSQ a -> (IntPSQ a, Maybe a)
deleteView k t0 =
    case delFrom t0 of
      (# t, mbX #) -> (t, mbX)
  where
    delFrom t = case t of
      Nil           -> (# Nil, Nothing #)
      Tip k' x'
        | k == k'   -> (# Nil, Just x' #)
        | otherwise -> (# t,   Nothing #)

      Bin k' x' m l r
        | nomatch k k' m -> (# t, Nothing #)
        | k == k'   -> let t' = merge m l r
                       in  t' `seq` (# t', Just x' #)

        | zero k m  -> case delFrom l of
                         (# l', mbX #) -> let t' = binShrinkL k' x' m l' r
                                          in  t' `seq` (# t', mbX #)

        | otherwise -> case delFrom r of
                         (# r', mbX #) -> let t' = binShrinkR k' x' m l  r'
                                          in  t' `seq` (# t', mbX #)


{-# INLINABLE delete #-}
delete :: Ord a => Key -> IntPSQ a -> IntPSQ a
delete k t = case t of
    Nil           -> Nil

    Tip k' x'
      | k == k'   -> Nil
      | otherwise -> t

    Bin k' x' m l r
      | nomatch k k' m -> t
      | k == k'        -> merge m l r
      | zero k m       -> binShrinkL k' x' m (delete k l) r
      | otherwise      -> binShrinkR k' x' m l            (delete k r)


toList :: IntPSQ a -> [(Int, a)]
toList =
    go []
  where
    go acc Nil                = acc
    go acc (Tip k' x')        = (k', x') : acc
    go acc (Bin k' x' _m l r) = (k', x') : go (go acc r) l


