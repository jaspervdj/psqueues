-- | A
module Data.HashPSQ
  ( HashPSQ
  , empty
  , null
  , insert
  , lookup
  , fromList
  ) where

import           Data.Hashable
import qualified Data.IntPSQ as IPSQ
import qualified Data.List   as L

import           Prelude hiding (lookup)

type HashPSQ k p v = IPSQ.IntPSQ p (k, v, [(k, p, v)])

lookup :: (Eq k, Hashable k, Ord p) => k -> HashPSQ k p v -> Maybe (p, v)
lookup k ipsq = do
    (p0, (k0, v0, es)) <- IPSQ.lookup (hash k) ipsq
    if k0 == k
      then do return (p0, v0)
      else do (_k', p', v') <- L.find (\(k', _, _) -> k' == k) es
              return (p', v')

empty :: HashPSQ k p v
empty = IPSQ.empty

insert :: (Eq k, Hashable k, Ord p)
       => k -> p -> v -> HashPSQ k p v -> HashPSQ k p v
insert k p v ipsq =
    IPSQ.alter_ ins (hash k) ipsq
  where
    ins Nothing                   = Just (p,  (k,  v,  []               ))
    ins (Just (p', (k', v', es)))
      | k == k                    = Just (p,  (k , v , []               ))
      | p' <= p                   = Just (p', (k', v', (k , p , v ) : es))
      | otherwise                 = Just (p , (k , v , (k', p', v') : es))

{-# INLINABLE fromList #-}
fromList :: (Eq k, Hashable k, Ord p) => [(k, p, v)] -> HashPSQ k p v
fromList = L.foldl' (\psq (k, p, x) -> insert k p x psq) empty


{-
{-# INLINE minViewWithKey #-}
minViewWithKey :: Ord p => HashPSQ k p v -> Maybe ((k, p, v), HashPSQ k p v)
minViewWithKey t = case t of
    Nil             -> Nothing
    Tip k p x       -> Just ((k, p, x), Nil)
    Bin k p x m l r -> Just ((k, p, x), merge m l r)
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
