{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.IntPSQ
    ( IntPSQ(..)
    , map
    , fromList
    , lookup
    , delete
    , insert
    , alter
    , alter_
    , alterMin
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

-- | A priority search queue with @Int@ keys and priorities of type @p@ and
-- values of type @v@. It is strict in keys, priorities and values.
data IntPSQ p v
    = Bin {-# UNPACK #-} !Key !p !v {-# UNPACK #-} !Mask !(IntPSQ p v) !(IntPSQ p v)
    | Tip {-# UNPACK #-} !Key !p !v
    | Nil
    deriving (Show)


-- instances
------------

instance (NFData p, NFData v) => NFData (IntPSQ p v) where
    rnf (Bin k p v _m l r) = rnf p `seq` rnf v `seq` rnf l `seq` rnf r
    rnf (Tip k p v)        = rnf p `seq` rnf v
    rnf Nil                = ()



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

null :: IntPSQ p v -> Bool
null Nil = True
null _   = False

lookup :: Key -> IntPSQ p v -> Maybe (p, v)
lookup k t = case t of
    Nil                -> Nothing

    Tip k' p' x'
      | k == k'        -> Just (p', x')
      | otherwise      -> Nothing

    Bin k' p' x' m l r
      | nomatch k k' m -> Nothing
      | k == k'        -> Just (p', x')
      | zero k m       -> lookup k l
      | otherwise      -> lookup k r


------------------------------------------------------------------------------
--- Construction
------------------------------------------------------------------------------


empty :: IntPSQ p v
empty = Nil

-- Insertion
------------

-- | Link
link :: Key -> p -> v -> Key -> IntPSQ p v -> IntPSQ p v -> IntPSQ p v
link k p x k' k't otherTree
  | zero m k' = Bin k p x m k't       otherTree
  | otherwise = Bin k p x m otherTree k't
  where
    m = branchMask k k'

-- | This variant of insert has the most consistent performance. It does at
-- most two root-to-leaf traversals, which are reallocating the nodes on their
-- path.
{-# INLINE insert #-}
insert :: Ord p => Key -> p -> v -> IntPSQ p v -> IntPSQ p v
insert k p x t0 = insertNew k p x (delete k t0)

{-# INLINE alter #-}
alter
    :: Ord p
    => (Maybe (p, v) -> (Maybe (p, v), b)) -> Key -> IntPSQ p v -> (IntPSQ p v, b)
alter mkNext k t0 =
    case deleteView k t0 of
      (t, mbPrioAndX) ->
        case mkNext mbPrioAndX of
          (Nothing,      result) -> (t,                 result)
          (Just (p, x),  result) -> (insertNew k p x t, result)

{-# INLINE alter_ #-}
alter_ :: Ord p
       => (Maybe (p, v) -> Maybe (p, v)) -> Key -> IntPSQ p v -> IntPSQ p v
alter_ mkNext k ipqs = fst (alter (\x -> (mkNext x, ())) k ipqs)

-- | Internal function to insert a key that is *not* present in the priority
-- queue.
{-# INLINABLE insertNew #-}
insertNew :: Ord p => Key -> p -> v -> IntPSQ p v -> IntPSQ p v
insertNew k p x t = case t of
  Nil       -> Tip k p x

  Tip k' p' x'
    | (p, k) < (p', k') -> link k  p  x  k' t           Nil
    | otherwise         -> link k' p' x' k  (Tip k p x) Nil

  Bin k' p' x' m l r
    | nomatch k k' m ->
        if (p, k) < (p', k')
          then link k  p  x  k' t           Nil
          else link k' p' x' k  (Tip k p x) (merge m l r)

    | otherwise ->
        if (p, k) < (p', k')
          then
            if zero k' m
              then Bin k  p  x  m (insertNew k' p' x' l) r
              else Bin k  p  x  m l                      (insertNew k' p' x' r)
          else
            if zero k m
              then Bin k' p' x' m (insertNew k  p  x  l) r
              else Bin k' p' x' m l                      (insertNew k  p  x  r)


-- | A supposedly more clever variant of insert that first looks up the key
-- and then re-establishes the min-heap property in a bottom-up fashion.
--
-- NOTE (SM): the performacne of this function is bad if there are many
-- priority decrements of keys that are deep down in the map. I think it might
-- even have a quadratic worst-case performance because of the repeated calls
-- to 'merge'.
{-# INLINABLE insert2 #-}
insert2 :: Ord p => Key -> p -> v -> IntPSQ p v -> IntPSQ p v
insert2 k p x =
    go
  where
    go t = case t of
      Nil -> Tip k p x

      Tip k' p' x'
        | k == k'           -> Tip k p x
        | (p, k) < (p', k') -> link k  p  x  k' t           Nil
        | otherwise         -> link k' p' x' k  (Tip k p x) Nil

      Bin k' p' x' m l r
        | nomatch k k' m ->
            if (p, k) < (p', k')
              then link k  p  x  k' t           Nil
              else link k' p' x' k  (Tip k p x) (merge m l r)

        | k == k' ->
            if p < p'
              then Bin k p x m l r
              else insertNew k p x (merge m l r)

        | zero k m  -> binBubbleL k' p' x' m (go l) r
        | otherwise -> binBubbleR k' p' x' m l      (go r)

-- | A smart constructor for a 'Bin' node whose left subtree's root could have
-- a smaller priority and therefore needs to be bubbled up.
{-# INLINE binBubbleL #-}
binBubbleL :: Ord p => Key -> p -> v -> Mask -> IntPSQ p v -> IntPSQ p v -> IntPSQ p v
binBubbleL k p x m l r = case l of
    Nil                   -> Bin k  p  x  m Nil                   r
    Tip lk lp lx
      | (p, k) < (lp, lk) -> Bin k  p  x  m l                     r
      | zero k m          -> Bin lk lp lx m (Tip k p x)           r
      | otherwise         -> Bin lk lp lx m Nil                   (insertNew k p x r)

    Bin lk lp lx lm ll lr
      | (p, k) < (lp, lk) -> Bin k  p  x  m l                     r
      | zero k m          -> Bin lk lp lx m (Bin k p  x lm ll lr) r
      | otherwise         -> Bin lk lp lx m (merge lm ll lr)      (insertNew k p x r)

-- | A smart constructor for a 'Bin' node whose right subtree's root could
-- have a smaller priority and therefore needs to be bubbled up.
{-# INLINE binBubbleR #-}
binBubbleR :: Ord p => Key -> p -> v -> Mask -> IntPSQ p v -> IntPSQ p v -> IntPSQ p v
binBubbleR k p x m l r = case r of
    Nil                   -> Bin k  p  x  m l                   Nil
    Tip rk rp rx
      | (p, k) < (rp, rk) -> Bin k  p  x  m l                   r
      | zero k m          -> Bin rk rp rx m (insertNew k p x l) Nil
      | otherwise         -> Bin rk rp rx m l                   (Tip k p x)

    Bin rk rp rx rm rl rr
      | (p, k) < (rp, rk) -> Bin k  p  x  m l                   r
      | zero k m          -> Bin rk rp rx m (insertNew k p x l) (merge rm rl rr)
                             -- NOTE that this case can be quite expensive, as
                             -- we might end up merging the same case multiple
                             -- times.
      | otherwise         -> Bin rk rp rx m l                   (Bin k p x rm rl rr)

-- | A variant of insert that fuses the delete pass and the insertNew pass and
-- does not need to re-establish the min-heap property in a bottom-up fashion.
--
-- NOTE (SM) surprisingly, it is slower in benchmarks, which might be cause it
-- is buggy, or because there's some bad Core being generated.
{-# INLINABLE insert3 #-}
insert3 :: Ord p => Key -> p -> v -> IntPSQ p v -> IntPSQ p v
insert3 k p x t = case t of
    Nil -> Tip k p x

    Tip k' p' x' ->
      case compare k k' of
        EQ -> Tip k' p x
        LT -> if p' <= p'
                then link k  p  x  k' t           Nil
                else link k' p' x' k  (Tip k p x) Nil
        GT -> if p < p'
                then link k  p  x  k' t           Nil
                else link k' p' x' k  (Tip k p x) Nil

    Bin k' p' x' m l r
      | nomatch k k' m ->
          if (p, k) < (p', k')
            then link k  p  x  k' t           Nil
            else link k' p' x' k  (Tip k p x) (merge m l r)

      | k == k' ->
          if p <= p'
            then Bin k' p x m l r
            else insertNew k p x (merge m l r)

      | (p, k) < (p', k') ->
          case (zero k m, zero k' m) of
            (False, False) -> Bin k p x m                               l   (insertNew k' p' x' (delete k r))
            (False, True ) -> Bin k p x m (insertNew k' p' x'           l )                     (delete k r)
            (True,  False) -> Bin k p x m                     (delete k l)  (insertNew k' p' x'           r )
            (True,  True ) -> Bin k p x m (insertNew k' p' x' (delete k l))                               r

      | otherwise ->
          if zero k m
            then Bin k' p' x' m (insert k p x l) r
            else Bin k' p' x' m l                (insert k p x r)



-- | Internal function that merges two *disjoint* 'IntPSQ's that share the
-- same prefix mask.
{-# INLINABLE merge #-}
merge :: Ord p => Mask -> IntPSQ p v -> IntPSQ p v -> IntPSQ p v
merge m l r = case l of
    Nil -> r

    Tip lk lp lx ->
      case r of
        Nil                     -> l
        Tip rk rp rx
          | (lp, lk) < (rp, rk) -> Bin lk lp lx m Nil r
          | otherwise           -> Bin rk rp rx m l   Nil
        Bin rk rp rx rm rl rr
          | (lp, lk) < (rp, rk) -> Bin lk lp lx m Nil r
          | otherwise           -> Bin rk rp rx m l   (merge rm rl rr)

    Bin lk lp lx lm ll lr ->
      case r of
        Nil                     -> l
        Tip rk rp rx
          | (lp, lk) < (rp, rk) -> Bin lk lp lx m (merge lm ll lr) r
          | otherwise           -> Bin rk rp rx m l                Nil
        Bin rk rp rx rm rl rr
          | (lp, lk) < (rp, rk) -> Bin lk lp lx m (merge lm ll lr) r
          | otherwise           -> Bin rk rp rx m l                (merge rm rl rr)



-- FromList
-----------

{-# INLINABLE fromList #-}
fromList :: Ord p => [(Key, p, v)] -> IntPSQ p v
fromList = foldl' (\im (k, p, x) -> insert k p x im) empty

{-# INLINABLE fromList2 #-}
fromList2 :: Ord p => [(Key, p, v)] -> IntPSQ p v
fromList2 = foldl' (\im (k, p, x) -> insert2 k p x im) empty

{-# INLINABLE fromList3 #-}
fromList3 :: Ord p => [(Key, p, v)] -> IntPSQ p v
fromList3 = foldl' (\im (k, p, x) -> insert3 k p x im) empty


------------------------------------------------------------------------------
-- Modification
------------------------------------------------------------------------------


{-# INLINABLE map #-}
map :: (v -> v') -> IntPSQ p v -> IntPSQ p v'
map f =
    go
  where
    go t = case t of
        Nil             -> Nil
        Tip k p x       -> Tip k p (f x)
        Bin k p x m l r -> Bin k p (f x) m (go l) (go r)


------------------------------------------------------------------------------
-- Destruction
------------------------------------------------------------------------------

{-# INLINE alterMin #-}
alterMin :: Ord p
         => (Maybe (Key, p, v) -> (b, Maybe (Key, p, v)))
         -> IntPSQ p v
         -> (b, IntPSQ p v)
alterMin f t = case t of
    Nil             -> case f Nothing of
                         (b, Nothing)           -> (b, Nil)
                         (b, Just (k', p', x')) -> (b, Tip k' p' x')

    Tip k p x       -> case f (Just (k, p, x)) of
                         (b, Nothing)           -> (b, Nil)
                         (b, Just (k', p', x')) -> (b, Tip k' p' x')

    Bin k p x m l r -> case f (Just (k, p, x)) of
                         (b, Nothing)           -> (b, merge m l r)
                         (b, Just (k', p', x'))
                           | k  /= k'  -> (b, insert k' p' x' (merge m l r))
                           | p' <= p   -> (b, Bin k p' x' m l r)
                           | otherwise -> (b, insertNew k p' x' (merge m l r))


{-# INLINE minViewWithKey #-}
minViewWithKey :: Ord p => IntPSQ p v -> Maybe ((Key, p, v), IntPSQ p v)
minViewWithKey t = case t of
    Nil             -> Nothing
    Tip k p x       -> Just ((k, p, x), Nil)
    Bin k p x m l r -> Just ((k, p, x), merge m l r)


-- | Smart constructor for a 'Bin' node whose left subtree could have become
-- 'Nil'.
{-# INLINE binShrinkL #-}
binShrinkL :: Key -> p -> v -> Mask -> IntPSQ p v -> IntPSQ p v -> IntPSQ p v
binShrinkL k p x m Nil r = case r of Nil -> Tip k p x; _ -> Bin k p x m Nil r
binShrinkL k p x m l   r = Bin k p x m l r

-- | Smart constructor for a 'Bin' node whose right subtree could have become
-- 'Nil'.
{-# INLINE binShrinkR #-}
binShrinkR :: Key -> p -> v -> Mask -> IntPSQ p v -> IntPSQ p v -> IntPSQ p v
binShrinkR k p x m l Nil = case l of Nil -> Tip k p x; _ -> Bin k p x m l Nil
binShrinkR k p x m l r   = Bin k p x m l r


-- TODO (SM): verify that it is really worth do do deletion and lookup at the
-- same time.
{-# INLINABLE deleteView #-}
deleteView :: Ord p => Key -> IntPSQ p v -> (IntPSQ p v, Maybe (p, v))
deleteView k t0 =
    case delFrom t0 of
      (# t, mbPX #) -> (t, mbPX)
  where
    delFrom t = case t of
      Nil -> (# Nil, Nothing #)

      Tip k' p' x'
        | k == k'   -> (# Nil, Just (p', x') #)
        | otherwise -> (# t,   Nothing       #)

      Bin k' p' x' m l r
        | nomatch k k' m -> (# t, Nothing #)
        | k == k'   -> let t' = merge m l r
                       in  t' `seq` (# t', Just (p', x') #)

        | zero k m  -> case delFrom l of
                         (# l', mbPX #) -> let t' = binShrinkL k' p' x' m l' r
                                           in  t' `seq` (# t', mbPX #)

        | otherwise -> case delFrom r of
                         (# r', mbPX #) -> let t' = binShrinkR k' p' x' m l  r'
                                           in  t' `seq` (# t', mbPX #)


{-# INLINABLE delete #-}
delete :: Ord p => Key -> IntPSQ p v -> IntPSQ p v
delete k t = case t of
    Nil           -> Nil

    Tip k' _ _
      | k == k'   -> Nil
      | otherwise -> t

    Bin k' p' x' m l r
      | nomatch k k' m -> t
      | k == k'        -> merge m l r
      | zero k m       -> binShrinkL k' p' x' m (delete k l) r
      | otherwise      -> binShrinkR k' p' x' m l            (delete k r)


toList :: IntPSQ p v -> [(Int, p, v)]
toList =
    go []
  where
    go acc Nil                = acc
    go acc (Tip k' p' x')        = (k', p', x') : acc
    go acc (Bin k' p' x' _m l r) = (k', p', x') : go (go acc r) l

