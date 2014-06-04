-- | A
module Data.HashPSQ
  ( HashPSQ
  , empty
  , null
  , insert
  , lookup
  , fromList
  , minView
  ) where

import           Data.Hashable
import qualified Data.IntPSQ as IPSQ
import qualified Data.List   as L

import           Prelude hiding (lookup)

------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

-- | TODO (SM): use an actual PSQ for this to make the datastructure resistant
-- against hash-collision attacks.
data Overflow k p v = OEmpty | OInsert !k !p !v (Overflow k p v)
    deriving (Show)

data Bucket k p v = B !k !v !(Overflow k p v)
    deriving (Show)

newtype HashPSQ k p v = HashPSQ (IPSQ.IntPSQ p (Bucket k p v))
    deriving (Show)



------------------------------------------------------------------------------
-- Overflow list functions
------------------------------------------------------------------------------

{-# INLINABLE olookup #-}
olookup :: Ord k => k -> Overflow k p v -> Maybe (p, v)
olookup k =
    go
  where
    go os = case os of
      OEmpty               -> Nothing
      OInsert k' p' v' os' -> case compare k k' of
        LT -> go os'
        EQ -> Just (p', v')
        GT -> Nothing

{-# INLINABLE odelete #-}
odelete :: Ord k => k -> Overflow k p v -> Overflow k p v
odelete k =
    go
  where
    go os = case os of
      OEmpty               -> OEmpty
      OInsert k' p' v' os' -> case compare k k' of
        LT -> OInsert k' p' v' (go os')
        EQ -> os'
        GT -> os

{-
{-# INLINABLE odelete #-}
odeleteView :: Ord k => k -> Overflow k p v -> (Maybe (p, v), Overflow k p v)
odeleteView k os0 =
    case go os0 of
      (# mbX, os0' #) -> (mbX, os0')
  where
    go os = case os of
      OEmpty               -> (# Nothing, OEmpty #)
      OInsert k' p' v' os' -> case compare k k' of
        LT -> case go os' of
                (# mbX, os'') -> let os''' = OInsert k' p' v' (go os')
                                 in os''' `seq` (# mbX, os''' #)
        EQ -> (# Just (p', v'), os'#)
        GT -> (# Nothing,       os #)
-}


{-# INLINABLE oinsert #-}
oinsert :: Ord k => k -> p -> v -> Overflow k p v -> Overflow k p v
oinsert k p v =
    go
  where
    go os = case os of
      OEmpty               -> OInsert k p v OEmpty
      OInsert k' p' v' os' -> case compare k k' of
        LT -> OInsert k p v os
        EQ -> OInsert k p v os'
        GT -> OInsert k' p' v' (go os')

{-# INLINABLE ominView #-}
ominView :: (Ord k, Ord p) => Overflow k p v -> (Maybe (k, p, v), Overflow k p v)
ominView OEmpty                = (Nothing, OEmpty)
ominView (OInsert k0 p0 v0 os) =
    case go k0 p0 v0 os of
      res@(k, _, _) -> (Just res, odelete k os)
  where
    go k p v OEmpty                             = (k, p, v)
    go k p v (OInsert k' p' v' os') | p <= p'   = go k  p  v  os'
                                    | otherwise = go k' p' v' os'

{-
{-# INLINABLE ominView #-}
oalter :: (Ord k, Ord p)
        => (Maybe (p, v) -> Maybe (k, p, v))
        -> k -> Overflow k p v -> Overflow k p v
oalter f os0 =
    case odeleteView k os0 of
      (os, mbX) ->
        case f mbX of
          (b, mbX') -> (b, maybe t (\(p, v) -> oinsert k p v t))

-- | Smart constructor for a bucket that
bucket ::
-}

------------------------------------------------------------------------------
-- HashPSQ functions
------------------------------------------------------------------------------

empty :: HashPSQ k p v
empty = HashPSQ IPSQ.empty

{-# INLINABLE lookup #-}
lookup :: (Ord k, Hashable k, Ord p) => k -> HashPSQ k p v -> Maybe (p, v)
lookup k (HashPSQ ipsq) = do
    (p0, B k0 v0 os) <- IPSQ.lookup (hash k) ipsq
    if k0 == k
      then return (p0, v0)
      else olookup k os

{-# INLINABLE insert #-}
insert :: (Ord k, Hashable k, Ord p)
       => k -> p -> v -> HashPSQ k p v -> HashPSQ k p v
insert k p v (HashPSQ ipsq) =
    HashPSQ (IPSQ.alter_ ins (hash k) ipsq)
  where
    ins Nothing                 = Just (p,  B k  v  (OEmpty             ))
    ins (Just (p', B k' v' os))
      | k == k                  = Just (p,  B k  v  (                 os))
      | p' <= p                 = Just (p', B k' v' (oinsert k  p  v  os))
      | otherwise               = Just (p , B k  v  (oinsert k' p' v' os))

{-# INLINABLE fromList #-}
fromList :: (Ord k, Hashable k, Ord p) => [(k, p, v)] -> HashPSQ k p v
fromList = L.foldl' (\psq (k, p, x) -> insert k p x psq) empty


{-# INLINABLE minView #-}
minView :: (Ord k, Hashable k, Ord p)
        => HashPSQ k p v -> (Maybe (k, p, v), HashPSQ k p v)
minView (HashPSQ ipsq )=
    case IPSQ.alterMin f ipsq of
      (res, ipsq') -> (res, HashPSQ ipsq')
  where
    f Nothing                  = (Nothing, Nothing)
    f (Just (_h, p, B k v os)) =
        case ominView os of
          (Nothing,           _os') -> (Just (k, p, v), Nothing                        )
          (Just (k', p', v'),  os') -> (Just (k, p, v), Just (hash k', p', B k' v' os'))
{-
{-# INLINABLE deleteView #-}
deleteView :: Ord k => k -> HashPSQ k p v -> (Maybe (p, v), HashPSQ k p v)
deleteView =
    alter del
  where
    del mbPV = (mbPV, Nothing )


{-# INLINE alter #-}
alter :: (Ord k, Hashable k, Ord p)
      => (Maybe (p, v) -> (b, Maybe (k, p, v)))
      -> k -> HashPSQ k p v -> (b, HashPSQ k p v)
alter f =
    \k (HashPSQ ipsq) -> HashPSQ (IPSQ.alter f' (hash k) ipsq)
  where
    f' mbX = case f mbX of
               (b, mbX') -> (b, insertIfNecessary mbX')

    unhash Nothing                 = f Nothing
    unhash (Just (p, B k' v' os'))
      | k' == k   =
      | otherwise = oalter g k

    insertIfNecessary Nothing ->

    Nothing                 = Just (p,  B k  v  (OEmpty             ))
    f' (Just (p', B k' v' os))
      | k == k                  = Just (p,  B k  v  (                 os))
      | p' <= p                 = Just (p', B k' v' (oinsert k  p  v  os))
      | otherwise               = Just (p , B k  v  (oinsert k' p' v' os))
-}

-- delete


{-
data Bucket k v
    = Empty
    | Insert !k !v !(Bucket k v)
    deriving (Show {-! Eq, Eq, Ord !-})

instance (Eq k, Eq v) => Eq (Bucket k v) where
        Empty           == Empty           = True
        Insert k1 v1 b1 == Insert k2 v2 b2 = v1 == v2 && k1 == k2 && b1 == b2
        _               == _               = False


instance (Ord k, Ord v) => Ord (Bucket k v) where
        compare b1 b2 =
            check a b
          where check Empty Empty = EQ
                check (Insert x1 x2 x3) (Insert y1 y2 y3)
                  = compare x1 y1 `_then` compare x2 y2 `_then` compare x3 y3 `_then`
                      EQ
                check x y = compare (tag x) (tag y)

                _then EQ x = x
                _then x _  = x

                tag (Empty{}) = 0 :: Int
                tag (Insert{}) = 1 :: Int


instance Show Entry where
        showsPrec p (Entry x1 x2 x3)
          = showParen (p > 10) $
              showString "Entry " .
                showsPrec 11 x1 .
                  showChar ' ' . showsPrec 11 x2 . showChar ' ' . showsPrec 11 x3


instance Eq Entry where
        Entry x1 x2 x3 == Entry y1 y2 y3 = x1 == y1 && x2 == y2 && x3 == y3


instance Ord Entry where
        compare a b = check a b
          where check (Entry x1 x2 x3) (Entry y1 y2 y3)
                  = compare x1 y1 `_then` compare x2 y2 `_then` compare x3 y3 `_then`
                      EQ
                _then EQ x = x
                _then x _ = x



data Entry = Entry !v !k !Bucket
    deriving (Show {-! Eq, Eq, Ord !-})

{-

{-# INLINABLE collision #-}
collision :: Ord v => v -> Bucket v -> Bucket v
collision


newtype HashPSQ = HashPSQ
    { unHashPSQ :: IM.IntPSQ
-}
-}
