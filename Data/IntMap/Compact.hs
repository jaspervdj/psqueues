{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
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
    , union
    , lookup
    , member
    , notMember
    , map

    -- * PSQ
    , IntPSQ
    , pfromList
    , plookup
    , pinsert
    , pempty
    , pminViewWithKey
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

instance Monoid (IntMap a) where
    mempty  = empty
    mappend = union
    -- mconcat = unions

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



union :: IntMap a -> IntMap a -> IntMap a
union l r = case l of
    Nil                -> r
    Tip lk lx          -> insert lk lx r
    Bin lk lx lm ll lr ->
      case r of
        Nil                -> l
        Tip rk rx          -> insert rk rx l
        Bin rk rx rm rl rr ->
          case compare lm rm of
            LT | nomatch lk rk rm -> link rk rx lk l (merge rm rl rr)
               | zero lk rm       -> Bin rk rx rm (union l rl) rr
               | otherwise        -> Bin rk rx rm rl           (union l rr)

            GT | nomatch rk lk lm -> link lk lx rk r (merge lm ll lr)
               | zero rk lm       -> Bin lk lx lm (union r ll) lr
               | otherwise        -> Bin lk lx lm ll           (union r lr)

            EQ -> insert rk rx (Bin lk lx lm (union ll rl) (union lr rr))



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


-- ===========================================================================
-- IntPSQ
-- ===========================================================================





data IntPSQ a
    = BIN {-# UNPACK #-} !Key a {-# UNPACK #-} !Mask !(IntPSQ a) !(IntPSQ a)
    | TIP {-# UNPACK #-} !Key a
    | NIL
    deriving (Eq, Show)


instance NFData a => NFData (IntPSQ a) where
    rnf NIL               = ()
    rnf (TIP _k x)        = rnf x
    rnf (BIN _k x _m l r) = rnf x `seq` rnf l `seq` rnf r

pempty :: IntPSQ a
pempty = NIL

pnull :: IntPSQ a -> Bool
pnull NIL = True
pnull _   = False


plookup :: Key -> IntPSQ a -> Maybe a
plookup k t = case t of
    NIL                -> Nothing

    TIP k' x'
      | k == k'        -> Just x'
      | otherwise      -> Nothing

    BIN k' x' m l r
      | nomatch k k' m -> Nothing
      | k == k'        -> Just x'
      | zero k m       -> plookup k l
      | otherwise      -> plookup k r


ptoList :: IntPSQ a -> [(Int, a)]
ptoList =
    go []
  where
    go acc NIL                = acc
    go acc (TIP k' x')        = (k', x') : acc
    go acc (BIN k' x' _m l r) = (k', x') : go (go acc r) l

-- | Smart constructor for a 'BIN' node whose left subtree could have become
-- 'NIL'.
{-# INLINE binl #-}
binl :: Key -> a -> Mask -> IntPSQ a -> IntPSQ a -> IntPSQ a
binl k x m NIL r = case r of NIL -> TIP k x; _ -> BIN k x m NIL r
binl k x m l   r = BIN k x m l r

-- | Smart constructor for a 'BIN' node whose right subtree could have become
-- 'NIL'.
{-# INLINE binr #-}
binr :: Key -> a -> Mask -> IntPSQ a -> IntPSQ a -> IntPSQ a
binr k x m l NIL = case l of NIL -> TIP k x; _ -> BIN k x m l NIL
binr k x m l r   = BIN k x m l r


-- TODO (SM): verify that it is really worth do do deletion and lookup at the
-- same time.
{-# INLINABLE pdeleteView #-}
pdeleteView :: Ord a => Key -> IntPSQ a -> (IntPSQ a, Maybe a)
pdeleteView k t0 =
    case delFrom t0 of
      (# t, mbX #) -> (t, mbX)
  where
    delFrom t = case t of
      NIL           -> (# NIL, Nothing #)
      TIP k' x'
        | k == k'   -> (# NIL, Just x' #)
        | otherwise -> (# t,   Nothing #)

      BIN k' x' m l r
        | nomatch k k' m -> (# t, Nothing #)
        | k == k'   -> let t' = pmerge m l r
                       in  t' `seq` (# t', Just x' #)

        | zero k m  -> case delFrom l of
                         (# l', mbX #) -> let t' = binl k' x' m l' r
                                          in  t' `seq` (# t', mbX #)

        | otherwise -> case delFrom r of
                         (# r', mbX #) -> let t' = binr k' x' m l  r'
                                          in  t' `seq` (# t', mbX #)

{-# INLINABLE pdelete #-}
pdelete :: Ord a => Key -> IntPSQ a -> IntPSQ a
pdelete k t = case t of
    NIL           -> NIL

    TIP k' x'
      | k == k'   -> NIL
      | otherwise -> t

    BIN k' x' m l r
      | nomatch k k' m -> t
      | k == k'        -> pmerge m l r
      | zero k m       -> binl k' x' m (pdelete k l) r
      | otherwise      -> binr k' x' m l             (pdelete k r)


{-# INLINE pinsert #-}
pinsert :: Ord a => Key -> a -> IntPSQ a -> IntPSQ a
pinsert k x t0 =
    case pdeleteView k t0 of
      (t, _mbX) -> insertNew k x t
    -- insertNew k x (pdelete k t)
    {-
    case plookup k t of
      Nothing   -> insertNew k x t
      Just _mbX -> insertNew k x (pdelete k t)
    -}


{-# INLINE pminViewWithKey #-}
pminViewWithKey :: Ord a => IntPSQ a -> Maybe ((Key, a), IntPSQ a)
pminViewWithKey t = case t of
    NIL           -> Nothing
    TIP k x       -> Just ((k, x), NIL)
    BIN k x m l r -> Just ((k, x), pmerge m l r)


{-# INLINE palter #-}
palter :: Ord a => (Maybe a -> (Maybe a, b)) -> Key -> IntPSQ a -> (IntPSQ a, b)
palter mkNextX k t0 =
    case pdeleteView k t0 of
      (t, mbX) ->
        case mkNextX mbX of
          (Nothing, result) -> (t,               result)
          (Just x,  result) -> (insertNew k x t, result)

-- | Insert a key that is *not* present in the priority queue.
{-# INLINABLE insertNew #-}
insertNew :: Ord a => Key -> a -> IntPSQ a -> IntPSQ a
insertNew k x t = case t of
  NIL       -> TIP k x

  TIP k' x'
    | (x, k) < (x', k') -> plink k  x  k' t         NIL
    | otherwise         -> plink k' x' k  (TIP k x) NIL

  BIN k' x' m l r
    | nomatch k k' m ->
        if (x, k) < (x', k')
          then plink k  x  k' t         NIL
          else plink k' x' k  (TIP k x) (pmerge m l r)

    | otherwise ->
        if (x, k) < (x', k')
          then
            if zero k' m
              then BIN k  x  m (insertNew k' x' l) r
              else BIN k  x  m l                 (insertNew k' x' r)
          else
            if zero k m
              then BIN k' x' m (insertNew k  x  l) r
              else BIN k' x' m l                 (insertNew k  x  r)

{-# INLINABLE pfromList #-}
pfromList :: Ord a => [(Key, a)] -> IntPSQ a
pfromList = foldl' (\im (k, x) -> pinsert k x im) pempty



{- A draft of a variant of insert that fuses the delete pass and the insertNew
 - pass. This is however quite complicated and looses out on the nice inlining
 - option that the two-pass combinations offer.

pinsert :: Ord a => Key -> a -> IntPSQ a -> IntPSQ a
pinsert k x t = case t of
    NIL       -> TIP k x

    TIP k' x' ->
      case compare k k' of
        EQ -> TIP k' x
        LT -> if x <= x'
                then plink k  x  k' t         NIL
                else plink k' x' k  (TIP k x) NIL
        GT -> if x < x'
                then plink k  x  k' t         NIL
                else plink k' x' k  (TIP k x) NIL

    BIN k' x' m l r
      | nomatch k k' m ->
          case compare x x' of
            LT             -> plink k  x  k' t         NIL
            GT             -> plink k' x' k  (TIP k x) (pmerge m l r)
            EQ | k < k'    -> plink k  x  k' t         NIL
               | otherwise -> plink k' x' k  (TIP k x) (pmerge m l r)

      | otherwise ->
          case compare k k' of
            EQ -> if x < x'
                    then BIN k' x m l r
                    else pinsert k x (pmerge m l r)

            LT | x <= x' ->
                  if zero k' m
                    then BIN k  x  m (pinsert k' x' l) r
                    else BIN k  x  m l                 (pinsert k' x' r)

               | otherwise ->
                  if zero k m
                    then BIN k' x' m (pinsert k  x  l) r
                    else BIN k' x' m l                 (pinsert k  x  r)

            GT | x <  x' ->
                  if zero k' m
                    then BIN k  x  m (pinsert k' x' l) r
                    else BIN k  x  m l                 (pinsert k' x' r)

               | otherwise ->
                  if zero k m
                    then BIN k' x' m (pinsert k  x  l) r
                    else BIN k' x' m l                 (pinsert k  x  r)

-}

plink :: Key -> a -> Key -> IntPSQ a -> IntPSQ a -> IntPSQ a
plink k x k' k't otherTree
  | zero m k' = BIN k x m k't       otherTree
  | otherwise = BIN k x m otherTree k't
  where
    m = branchMask k k'

-- | Internal function that merges two *disjoint* 'IntPSQ's that share the
-- same prefix mask.
{-# INLINABLE pmerge #-}
pmerge :: Ord a => Mask -> IntPSQ a -> IntPSQ a -> IntPSQ a
pmerge m l r = case l of
    NIL -> r

    TIP lk lx ->
      case r of
        NIL           -> l
        TIP rk rx
          | (lx, lk) < (rx, rk) -> BIN lk lx m NIL r
          | otherwise           -> BIN rk rx m l   NIL
        BIN rk rx rm rl rr
          | (lx, lk) < (rx, rk) -> BIN lk lx m NIL r
          | otherwise           -> BIN rk rx m l   (pmerge rm rl rr)

    BIN lk lx lm ll lr ->
      case r of
        NIL           -> l
        TIP rk rx
          | (lx, lk) < (rx, rk) -> BIN lk lx m (pmerge lm ll lr) r
          | otherwise           -> BIN rk rx m l                NIL
        BIN rk rx rm rl rr
          | (lx, lk) < (rx, rk) -> BIN lk lx m (pmerge lm ll lr) r
          | otherwise           -> BIN rk rx m l                (pmerge rm rl rr)
