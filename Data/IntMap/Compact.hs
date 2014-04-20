{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
module Data.IntMap.Compact
    ( IntMap
    , (!)
    , null
    , size
    , empty
    , singleton
    , fromList
    , toList
    , insert
    , lookup
    , member
    , notMember
    , map
    ) where

import Control.Applicative (Applicative(pure, (<*>)), (<$>))
import Control.DeepSeq (NFData(rnf))
import Control.Monad (liftM)
import Data.BitUtil
import Data.Bits
import qualified Data.Foldable as Foldable
import Data.List (foldl')
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid (Monoid(..))
import Data.Traversable (Traversable(traverse))
import Data.Typeable
import Data.Word (Word)
import Prelude hiding (lookup, map, filter, foldr, foldl, null)


-- Use macros to define strictness of functions.
-- STRICT_x_OF_y denotes an y-ary function strict in the x-th parameter.
-- We do not use BangPatterns, because they are not in any standard and we
-- want the compilers to be compiled by as many compilers as possible.
#define STRICT_1_OF_2(fn) fn arg _ | arg `seq` False = undefined

-- A "Nat" is a natural machine word (an unsigned Int)
type Nat = Word

type Key = Int

-- | We store masks as the index of the bit that determines the branching.
type Mask = Int


-- -- | True if the two keys are equal up to the mask bit.
-- nomatch, match :: Key -> Key -> Mask -> Bool
-- nomatch k1 k2 m = (natFromInt k1 >> m) /= (natFromInt k2 >> m)
-- match   k1 k2 m = (natFromInt k1 >> m) == (natFromInt k2 >> m)
--
-- zero :: Key -> Mask -> Bool
-- zero k m = k .&&. (1 << (m - 1)) == 0


data IntMap a
    = Bin {-# UNPACK #-} !Key a {-# UNPACK #-} !Mask !(IntMap a) !(IntMap a)
    | Tip {-# UNPACK #-} !Key a
    | Nil
    deriving (Eq, Show)


instance NFData a => NFData (IntMap a) where
    rnf Nil               = ()
    rnf (Tip _k x)        = rnf x
    rnf (Bin _k x _m l r) = rnf x `seq` rnf l `seq` rnf r

empty :: IntMap a
empty = Nil

null :: IntMap a -> Bool
null Nil = True
null _   = False


lookup :: Key -> IntMap a -> Maybe a
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


toList :: IntMap a -> [(Int, a)]
toList =
    go []
  where
    go acc Nil                = acc
    go acc (Tip k' x')        = (k', x') : acc
    go acc (Bin k' x' _m l r) = (k', x') : go (go acc r) l


insert :: Key -> a -> IntMap a -> IntMap a
insert k x t = case t of
    Nil       -> Tip k x

    Tip k' x' ->
      case compare k k' of
        EQ -> Tip k' x
        LT -> link k  x  k' t         Nil
        GT -> link k' x' k  (Tip k x) Nil

    Bin k' x' m l r
      | nomatch k k' m ->
          if k < k'
            then link k  x  k' t         Nil
            else link k' x' k  (Tip k x) (merge m l r)

      | otherwise ->
          case compare k k' of
            EQ -> Bin k' x m l r
            LT
              | zero k' m -> Bin k  x  m (insert k' x' l) r
              | otherwise -> Bin k  x  m l                (insert k' x' r)
            GT
              | zero k m  -> Bin k' x' m (insert k  x  l) r
              | otherwise -> Bin k' x' m l                (insert k  x  r)


link :: Key -> a -> Key -> IntMap a -> IntMap a -> IntMap a
link k x k' k't otherTree
  | zero m k' = Bin k x m k't       otherTree
  | otherwise = Bin k x m otherTree k't
  where
    m = branchMask k k'

merge :: Mask -> IntMap a -> IntMap a -> IntMap a
merge m l r = case l of
    Nil -> r

    Tip lk lx ->
      case r of
        Nil           -> l
        Tip rk rx
          | lk < rk   -> Bin lk lx m Nil r
          | otherwise -> Bin rk rx m l   Nil
        Bin rk rx rm rl rr
          | lk < rk   -> Bin lk lx m Nil r
          | otherwise -> Bin rk rx m l   (merge rm rl rr)

    Bin lk lx lm ll lr ->
      case r of
        Nil           -> l
        Tip rk rx
          | lk < rk   -> Bin lk lx m (merge lm ll lr) r
          | otherwise -> Bin rk rx m l                Nil
        Bin rk rx rm rl rr
          | lk < rk   -> Bin lk lx m (merge lm ll lr) r
          | otherwise -> Bin rk rx m l                (merge rm rl rr)

size :: IntMap a -> Int
size =
    go 0
  where
    go !s t = case t of
      Nil           -> s
      Tip _ _       -> 1 + s
      Bin _ _ _ l r -> go (go (1 + s) l) r


member :: Key -> IntMap a -> Bool
member k = isJust . lookup k

notMember :: Key -> IntMap a -> Bool
notMember k = not . isJust . lookup k

(!) :: IntMap a -> Key -> a
(!) im k = fromMaybe (error "IntMap.(!): key not found") $ lookup k im

fromList :: [(Key, a)] -> IntMap a
fromList = foldl' (\im (k, x) -> insert k x im) empty

singleton :: Key -> a -> IntMap a
singleton = Tip

map :: (a -> b) -> IntMap a -> IntMap b
map f =
    go
  where
    go Nil             = Nil
    go (Tip k x)       = Tip k (f x)
    go (Bin k x m l r) = Bin k (f x) m (go l) (go r)


{--------------------------------------------------------------------
  Endian independent bit twiddling
--------------------------------------------------------------------}

natFromInt :: Key -> Nat
natFromInt = fromIntegral
{-# INLINE natFromInt #-}

intFromNat :: Nat -> Key
intFromNat = fromIntegral
{-# INLINE intFromNat #-}

{--------------------------------------------------------------------
  Endian independent bit twiddling
--------------------------------------------------------------------}
zero :: Key -> Mask -> Bool
zero i m
  = (natFromInt i) .&. (natFromInt m) == 0
{-# INLINE zero #-}

nomatch, match :: Key -> Key -> Mask -> Bool
{-# INLINE nomatch #-}
{-# INLINE match   #-}
nomatch k1 k2 m =
    natFromInt k1 .&. m' /= natFromInt k2 .&. m'
  where
    m' = maskW (natFromInt m)

match k1 k2 m =
    natFromInt k1 .&. m' == natFromInt k2 .&. m'
  where
    m' = maskW (natFromInt m)

{--------------------------------------------------------------------
  Big endian operations
--------------------------------------------------------------------}
maskW :: Nat -> Nat
maskW m = complement (m-1) `xor` m
{-# INLINE maskW #-}

shorter :: Mask -> Mask -> Bool
shorter m1 m2
  = (natFromInt m1) > (natFromInt m2)
{-# INLINE shorter #-}

branchMask :: Key -> Key -> Mask
branchMask p1 p2
  = intFromNat (highestBitMask (natFromInt p1 `xor` natFromInt p2))
{-# INLINE branchMask #-}

