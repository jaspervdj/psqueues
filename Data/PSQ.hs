
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns, NoImplicitPrelude #-}

-- Copyright (c) 2008, Ralf Hinze
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
--     * Redistributions of source code must retain the above
--       copyright notice, this list of conditions and the following
--       disclaimer.
--
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials
--       provided with the distribution.
--
--     * The names of the contributors may not be used to endorse or
--       promote products derived from this software without specific
--       prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
-- FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
-- COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
-- INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
-- (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
-- OF THE POSSIBILITY OF SUCH DAMAGE.

-- | A /priority search queue/ (henceforth /queue/) efficiently
-- supports the operations of both a search tree and a priority queue.
-- An 'Elem'ent is a product of a key, a priority, and a
-- value. Elements can be inserted, deleted, modified and queried in
-- logarithmic time, and the element with the least priority can be
-- retrieved in constant time.  A queue can be built from a list of
-- elements, sorted by keys, in linear time.
--
-- This implementation is due to Ralf Hinze with some modifications by
-- Scott Dillard and Johan Tibell.
--
-- * Hinze, R., /A Simple Implementation Technique for Priority Search
-- Queues/, ICFP 2001, pp. 110-121
--
-- <http://citeseer.ist.psu.edu/hinze01simple.html>
module Data.PSQ
    ( PSQ

      -- * Query
    , null
    , size
    -- , member
    , lookup
    , findMin

      -- * Construction
    , empty
    , singleton

      -- * Insertion
    , insert

      -- * Delete/Update
    , delete
    -- , alter
    -- , alterMin

      -- * Conversion
    , fromList
    , toList
    -- , keys

    -- * Min
    , findMin
    , deleteMin

      -- * Views
    , deleteView
    , minView

      -- * Traversals
    -- , map
    , fold
    ) where

import Prelude ()
import Control.DeepSeq (NFData(rnf))
import Data.Maybe (Maybe(..))
import GHC.Base
import GHC.Num (Num(..))
import GHC.Show (Show(showsPrec))


-- | @E k p v@ binds the key @k@ to the value @v@ with priority @p@.
data Elem k p v = E
    { _key   :: !k
    , prio   :: !p
    , _value :: !v
    } deriving (Eq, Show)

instance (NFData k, NFData p, NFData v) => NFData (Elem k p v) where
    rnf (E k p v) = rnf k `seq` rnf p `seq` rnf v


unElem :: Elem k p v -> (k, p, v)
unElem (E k p v) = (k, p, v)


------------------------------------------------------------------------
-- | A mapping from keys @k@ to priorites @p@.

data PSQ k p v
    = Void
    | Winner !(Elem k p v)
             !(LTree k p v)
             !k
    deriving (Eq, Show)

instance (NFData k, NFData p, NFData v) => NFData (PSQ k p v) where
    rnf Void           = ()
    rnf (Winner e t m) = rnf e `seq` rnf m `seq` rnf t

-- | /O(1)/ The number of elements in a queue.
size :: (Ord p) => PSQ k p v -> Int
size Void            = 0
size (Winner _ lt _) = 1 + size' lt

-- | /O(1)/ True if the queue is empty.
null :: PSQ k p v -> Bool
null Void           = True
null (Winner _ _ _) = False

-- | /O(log n)/ The priority and value of a given key, or Nothing if
-- the key is not bound.
lookup :: (Ord k) => k -> PSQ k p v -> Maybe (p, v)
lookup k q = case tourView q of
    Null -> Nothing
    Single (E k' p v)
        | k == k'   -> Just (p, v)
        | otherwise -> Nothing
    tl `Play` tr
        | k <= maxKey tl -> lookup k tl
        | otherwise      -> lookup k tr

------------------------------------------------------------------------
-- Construction

empty :: PSQ k p v
empty = Void

-- | /O(1)/ Build a queue with one element.
singleton :: k -> p -> v -> PSQ k p v
singleton k p v = Winner (E k p v) Start k

------------------------------------------------------------------------
-- Insertion

-- | /O(log n)/ Insert a new key, priority and value in the queue.  If
-- the key is already present in the queue, the associated priority
-- and value are replaced with the supplied priority and value.
insert :: (Ord k, Ord p) => k -> p -> v -> PSQ k p v -> PSQ k p v
insert k p v q = case q of
    Void -> singleton k p v
    Winner (E k' p' v') Start _ -> case compare k k' of
        LT -> singleton k  p  v  `play` singleton k' p' v'
        EQ -> singleton k  p  v
        GT -> singleton k' p' v' `play` singleton k  p  v
    Winner e (RLoser _ e' tl m tr) m'
        | k <= m    -> insert k p v (Winner e tl m) `play` (Winner e' tr m')
        | otherwise -> (Winner e tl m) `play` insert k p v (Winner e' tr m')
    Winner e (LLoser _ e' tl m tr) m'
        | k <= m    -> insert k p v (Winner e' tl m) `play` (Winner e tr m')
        | otherwise -> (Winner e' tl m) `play` insert k p v (Winner e tr m')

{-# INLINABLE deleteView #-}
deleteView :: (Ord k, Ord p) => k -> PSQ k p v -> Maybe (p, v, PSQ k p v)
deleteView k psq = case psq of
    Void            -> Nothing
    Winner (E k' p v) Start _ 
        | k == k'   -> Just (p, v, empty)
        | otherwise -> Nothing
    Winner e (RLoser _ e' tl m tr) m'  
        | k <= m    -> fmap (\(p,v,q) -> (p, v,  q `play` (Winner e' tr m'))) (deleteView k (Winner e tl m))
        | otherwise -> fmap (\(p,v,q) -> (p, v,  (Winner e tl m) `play` q  )) (deleteView k (Winner e' tr m'))
    Winner e (LLoser _ e' tl m tr) m' 
        | k <= m    -> fmap (\(p,v,q) -> (p, v, q `play` (Winner e' tr m'))) (deleteView k (Winner e' tl m))
        | otherwise -> fmap (\(p,v,q) -> (p, v, (Winner e' tl m) `play` q )) (deleteView k (Winner e tr m'))


------------------------------------------------------------------------
-- Delete/Update

-- | /O(log n)/ Delete a key and its priority and value from the
-- queue.  When the key is not a member of the queue, the original
-- queue is returned.
delete :: (Ord k, Ord p) => k -> PSQ k p v -> PSQ k p v
delete k q = case q of
    Void -> empty
    Winner (E k' p v) Start _
        | k == k'   -> empty
        | otherwise -> singleton k' p v
    Winner e (RLoser _ e' tl m tr) m'
        | k <= m    -> delete k (Winner e tl m) `play` (Winner e' tr m')
        | otherwise -> (Winner e tl m) `play` delete k (Winner e' tr m')
    Winner e (LLoser _ e' tl m tr) m'
        | k <= m    -> delete k (Winner e' tl m) `play` (Winner e tr m')
        | otherwise -> (Winner e' tl m) `play` delete k (Winner e tr m')

-- | /O(log n)/ Update a priority at a specific key with the result
-- of the provided function.  When the key is not a member of the
-- queue, the original queue is returned.
adjust :: (Ord k, Ord p) => (p -> p) -> k -> PSQ k p v -> PSQ k p v
adjust f k q0 =  go q0
  where
    go q = case q of
        Void -> empty
        Winner (E k' p v) Start _
            | k == k'   -> singleton k' (f p) v
            | otherwise -> singleton k' p v
        Winner e (RLoser _ e' tl m tr) m'
            | k <= m    -> go (Winner e tl m) `unsafePlay` (Winner e' tr m')
            | otherwise -> (Winner e tl m) `unsafePlay` go (Winner e' tr m')
        Winner e (LLoser _ e' tl m tr) m'
            | k <= m    -> go (Winner e' tl m) `unsafePlay` (Winner e tr m')
            | otherwise -> (Winner e' tl m) `unsafePlay` go (Winner e tr m')
{-# INLINE adjust #-}

------------------------------------------------------------------------
-- Conversion

-- | /O(n*log n)/ Build a queue from a list of key/priority/value
-- tuples.  If the list contains more than one priority and value for
-- the same key, the last priority and value for the key is retained.
fromList :: (Ord k, Ord p) => [(k, p, v)] -> PSQ k p v
fromList = foldr (\(k, p, v) q -> insert k p v q) empty

-- | /O(n)/ Convert to a list of key/priority/value tuples.
toList :: PSQ k p v -> [(k, p, v)]
toList = toAscList

-- | /O(n)/ Convert to an ascending list.
toAscList :: PSQ k p v -> [(k, p, v)]
toAscList q  = seqToList (toAscLists q)

toAscLists :: PSQ k p v -> Sequ (k, p, v)
toAscLists q = case tourView q of
    Null             -> emptySequ
    Single (E k p v) -> singleSequ (k, p, v)
    tl `Play` tr     -> toAscLists tl <> toAscLists tr

-- | /O(n)/ Convert to a descending list.
toDescList :: PSQ k p v -> [(k, p, v)]
toDescList q = seqToList (toDescLists q)

toDescLists :: PSQ k p v -> Sequ (k, p, v)
toDescLists q = case tourView q of
    Null             -> emptySequ
    Single (E k p v) -> singleSequ (k, p, v)
    tl `Play` tr     -> toDescLists tr <> toDescLists tl

------------------------------------------------------------------------
-- Traversals

fold :: (Ord p) => (k -> p -> v -> a -> a) -> a -> PSQ k p v -> a
fold f acc =
    let
        fold_tree acc Start = acc
        fold_tree acc (LLoser _ e lt _ rt) = fold_tree' acc (e, lt, rt)
        fold_tree acc (RLoser _ e lt _ rt) = fold_tree' acc (e, lt, rt)

        fold_tree' acc ((E k p v), lt, rt) =
            let
                lta = fold_tree acc lt
                rta = fold_tree lta rt
            in
                f k p v rta

        go Void = acc
        go (Winner _ t _) = fold_tree acc t
    in
        go

------------------------------------------------------------------------
-- Min

-- | /O(1)/ The element with the lowest priority.
findMin :: PSQ k p v -> Maybe (k, p, v)
findMin Void           = Nothing
findMin (Winner e _ _) = Just (unElem e)

-- | /O(log n)/ Delete the element with the lowest priority.  Returns
-- an empty queue if the queue is empty.
deleteMin :: (Ord p) => PSQ k p v -> PSQ k p v
deleteMin Void           = Void
deleteMin (Winner _ t m) = secondBest t m

-- | /O(log n)/ Retrieve the binding with the least priority, and the
-- rest of the queue stripped of that binding.
minView :: (Ord p) => PSQ k p v -> Maybe (k, p, v, PSQ k p v)
minView Void           = Nothing
minView (Winner e t m) = let (k,p,v) = unElem e in
                           Just (k, p, v, secondBest t m)

secondBest :: (Ord p) => LTree k p v -> k -> PSQ k p v
secondBest Start _                 = Void
secondBest (LLoser _ e tl m tr) m' = Winner e tl m `play` secondBest tr m'
secondBest (RLoser _ e tl m tr) m' = secondBest tl m `play` Winner e tr m'

-- | /O(r*(log n - log r))/ Return a list of elements ordered by
-- key whose priorities are at most @pt@.
atMost :: (Ord p) => p -> PSQ k p v -> ([Elem k p v], PSQ k p v)
atMost pt q = let (sequ, q') = atMosts pt q
              in (seqToList sequ, q')

atMosts :: (Ord p) => p -> PSQ k p v -> (Sequ (Elem k p v), PSQ k p v)
atMosts !pt q = case q of
    (Winner e _ _)
        | prio e > pt -> (emptySequ, q)
    Void              -> (emptySequ, Void)
    Winner e Start _  -> (singleSequ e, Void)
    Winner e (RLoser _ e' tl m tr) m' ->
        let (sequ, q')   = atMosts pt (Winner e tl m)
            (sequ', q'') = atMosts pt (Winner e' tr m')
        in (sequ <> sequ', q' `play` q'')
    Winner e (LLoser _ e' tl m tr) m' ->
        let (sequ, q')   = atMosts pt (Winner e' tl m)
            (sequ', q'') = atMosts pt (Winner e tr m')
        in (sequ <> sequ', q' `play` q'')

------------------------------------------------------------------------
-- Loser tree

type Size = Int

data LTree k p v
    = Start
    | LLoser {-# UNPACK #-} !Size
             {-# UNPACK #-} !(Elem k p v)
                            !(LTree k p v)
                            !k              -- split key
                            !(LTree k p v)
    | RLoser {-# UNPACK #-} !Size
             {-# UNPACK #-} !(Elem k p v)
                            !(LTree k p v)
                            !k              -- split key
                            !(LTree k p v)
    deriving (Eq, Show)

instance (NFData k, NFData p, NFData v) => NFData (LTree k p v) where
    rnf Start              = ()
    rnf (LLoser _ e l k r) = rnf e `seq` rnf l `seq` rnf k `seq` rnf r
    rnf (RLoser _ e l k r) = rnf e `seq` rnf l `seq` rnf k `seq` rnf r



size' :: LTree k p v -> Size
size' Start              = 0
size' (LLoser s _ _ _ _) = s
size' (RLoser s _ _ _ _) = s

left, right :: LTree k p v -> LTree k p v

left Start                = moduleError "left" "empty loser tree"
left (LLoser _ _ tl _ _ ) = tl
left (RLoser _ _ tl _ _ ) = tl

right Start                = moduleError "right" "empty loser tree"
right (LLoser _ _ _  _ tr) = tr
right (RLoser _ _ _  _ tr) = tr

maxKey :: PSQ k p v -> k
maxKey Void           = moduleError "maxKey" "empty queue"
maxKey (Winner _ _ m) = m

lloser, rloser :: k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
lloser k p v tl m tr = LLoser (1 + size' tl + size' tr) (E k p v) tl m tr
rloser k p v tl m tr = RLoser (1 + size' tl + size' tr) (E k p v) tl m tr

------------------------------------------------------------------------
-- Balancing

-- | Balance factor
omega :: Int
omega = 4

lbalance, rbalance :: (Ord p) => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
lbalance k p v l m r
    | size' l + size' r < 2     = lloser        k p v l m r
    | size' r > omega * size' l = lbalanceLeft  k p v l m r
    | size' l > omega * size' r = lbalanceRight k p v l m r
    | otherwise                 = lloser        k p v l m r

rbalance k p v l m r
    | size' l + size' r < 2     = rloser        k p v l m r
    | size' r > omega * size' l = rbalanceLeft  k p v l m r
    | size' l > omega * size' r = rbalanceRight k p v l m r
    | otherwise                 = rloser        k p v l m r

lbalanceLeft :: (Ord p) => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
lbalanceLeft  k p v l m r
    | size' (left r) < size' (right r) = lsingleLeft  k p v l m r
    | otherwise                        = ldoubleLeft  k p v l m r

lbalanceRight :: (Ord p) => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
lbalanceRight k p v l m r
    | size' (left l) > size' (right l) = lsingleRight k p v l m r
    | otherwise                        = ldoubleRight k p v l m r

rbalanceLeft :: (Ord p) => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
rbalanceLeft  k p v l m r
    | size' (left r) < size' (right r) = rsingleLeft  k p v l m r
    | otherwise                        = rdoubleLeft  k p v l m r

rbalanceRight :: (Ord p) => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
rbalanceRight k p v l m r
    | size' (left l) > size' (right l) = rsingleRight k p v l m r
    | otherwise                        = rdoubleRight k p v l m r

lsingleLeft :: (Ord p) => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
lsingleLeft k1 p1 v1 t1 m1 (LLoser _ (E k2 p2 v2) t2 m2 t3)
    | p1 <= p2  = lloser k1 p1 v1 (rloser k2 p2 v2 t1 m1 t2) m2 t3
    | otherwise = lloser k2 p2 v2 (lloser k1 p1 v1 t1 m1 t2) m2 t3
lsingleLeft k1 p1 v1 t1 m1 (RLoser _ (E k2 p2 v2) t2 m2 t3) =
    rloser k2 p2 v2 (lloser k1 p1 v1 t1 m1 t2) m2 t3
lsingleLeft _ _ _ _ _ _ = moduleError "lsingleLeft" "malformed tree"

rsingleLeft :: k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
rsingleLeft k1 p1 v1 t1 m1 (LLoser _ (E k2 p2 v2) t2 m2 t3) =
    rloser k1 p1 v1 (rloser k2 p2 v2 t1 m1 t2) m2 t3
rsingleLeft k1 p1 v1 t1 m1 (RLoser _ (E k2 p2 v2) t2 m2 t3) =
    rloser k2 p2 v2 (rloser k1 p1 v1 t1 m1 t2) m2 t3
rsingleLeft _ _ _ _ _ _ = moduleError "rsingleLeft" "malformed tree"

lsingleRight :: k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
lsingleRight k1 p1 v1 (LLoser _ (E k2 p2 v2) t1 m1 t2) m2 t3 =
    lloser k2 p2 v2 t1 m1 (lloser k1 p1 v1 t2 m2 t3)
lsingleRight k1 p1 v1 (RLoser _ (E k2 p2 v2) t1 m1 t2) m2 t3 =
    lloser k1 p1 v1 t1 m1 (lloser k2 p2 v2 t2 m2 t3)
lsingleRight _ _ _ _ _ _ = moduleError "lsingleRight" "malformed tree"

rsingleRight :: (Ord p) => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
rsingleRight k1 p1 v1 (LLoser _ (E k2 p2 v2) t1 m1 t2) m2 t3 =
    lloser k2 p2 v2 t1 m1 (rloser k1 p1 v1 t2 m2 t3)
rsingleRight k1 p1 v1 (RLoser _ (E k2 p2 v2) t1 m1 t2) m2 t3
    | p1 <= p2  = rloser k1 p1 v1 t1 m1 (lloser k2 p2 v2 t2 m2 t3)
    | otherwise = rloser k2 p2 v2 t1 m1 (rloser k1 p1 v1 t2 m2 t3)
rsingleRight _ _ _ _ _ _ = moduleError "rsingleRight" "malformed tree"

ldoubleLeft :: (Ord p) => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
ldoubleLeft k1 p1 v1 t1 m1 (LLoser _ (E k2 p2 v2) t2 m2 t3) =
    lsingleLeft k1 p1 v1 t1 m1 (lsingleRight k2 p2 v2 t2 m2 t3)
ldoubleLeft k1 p1 v1 t1 m1 (RLoser _ (E k2 p2 v2) t2 m2 t3) =
    lsingleLeft k1 p1 v1 t1 m1 (rsingleRight k2 p2 v2 t2 m2 t3)
ldoubleLeft _ _ _ _ _ _ = moduleError "ldoubleLeft" "malformed tree"

ldoubleRight :: (Ord p) => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
ldoubleRight k1 p1 v1 (LLoser _ (E k2 p2 v2) t1 m1 t2) m2 t3 =
    lsingleRight k1 p1 v1 (lsingleLeft k2 p2 v2 t1 m1 t2) m2 t3
ldoubleRight k1 p1 v1 (RLoser _ (E k2 p2 v2) t1 m1 t2) m2 t3 =
    lsingleRight k1 p1 v1 (rsingleLeft k2 p2 v2 t1 m1 t2) m2 t3
ldoubleRight _ _ _ _ _ _ = moduleError "ldoubleRight" "malformed tree"

rdoubleLeft :: (Ord p) => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
rdoubleLeft k1 p1 v1 t1 m1 (LLoser _ (E k2 p2 v2) t2 m2 t3) =
    rsingleLeft k1 p1 v1 t1 m1 (lsingleRight k2 p2 v2 t2 m2 t3)
rdoubleLeft k1 p1 v1 t1 m1 (RLoser _ (E k2 p2 v2) t2 m2 t3) =
    rsingleLeft k1 p1 v1 t1 m1 (rsingleRight k2 p2 v2 t2 m2 t3)
rdoubleLeft _ _ _ _ _ _ = moduleError "rdoubleLeft" "malformed tree"

rdoubleRight :: (Ord p) => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
rdoubleRight k1 p1 v1 (LLoser _ (E k2 p2 v2) t1 m1 t2) m2 t3 =
    rsingleRight k1 p1 v1 (lsingleLeft k2 p2 v2 t1 m1 t2) m2 t3
rdoubleRight k1 p1 v1 (RLoser _ (E k2 p2 v2) t1 m1 t2) m2 t3 =
    rsingleRight k1 p1 v1 (rsingleLeft k2 p2 v2 t1 m1 t2) m2 t3
rdoubleRight _ _ _ _ _ _ = moduleError "rdoubleRight" "malformed tree"

-- | Take two pennants and returns a new pennant that is the union of
-- the two with the precondition that the keys in the ﬁrst tree are
-- strictly smaller than the keys in the second tree.
play :: (Ord p) => PSQ k p v -> PSQ k p v -> PSQ k p v
Void `play` t' = t'
t `play` Void  = t
Winner e@(E k p v) t m `play` Winner e'@(E k' p' v') t' m'
    | p <= p'   = Winner e (rbalance k' p' v' t m t') m'
    | otherwise = Winner e' (lbalance k p v t m t') m'
{-# INLINE play #-}

-- | A version of 'play' that can be used if the shape of the tree has
-- not changed or if the tree is known to be balanced.
unsafePlay :: (Ord p) => PSQ k p v -> PSQ k p v -> PSQ k p v
Void `unsafePlay` t' =  t'
t `unsafePlay` Void  =  t
Winner e@(E k p v) t m `unsafePlay` Winner e'@(E k' p' v') t' m'
    | p <= p'   = Winner e (rloser k' p' v' t m t') m'
    | otherwise = Winner e' (lloser k p v t m t') m'
{-# INLINE unsafePlay #-}

data TourView k p v = Null
                | Single {-# UNPACK #-} !(Elem k p v)
                | (PSQ k p v) `Play` (PSQ k p v)

tourView :: PSQ k p v -> TourView k p v
tourView Void               = Null
tourView (Winner e Start _) = Single e
tourView (Winner e (RLoser _ e' tl m tr) m') =
    Winner e tl m `Play` Winner e' tr m'
tourView (Winner e (LLoser _ e' tl m tr) m') =
    Winner e' tl m `Play` Winner e tr m'

------------------------------------------------------------------------
-- Utility functions

moduleError :: String -> String -> a
moduleError fun msg = error ("GHC.Event.PSQ." ++ fun ++ ':' : ' ' : msg)
{-# NOINLINE moduleError #-}

------------------------------------------------------------------------
-- Hughes's efficient sequence type

newtype Sequ a = Sequ ([a] -> [a])

emptySequ :: Sequ a
emptySequ = Sequ (\as -> as)

singleSequ :: a -> Sequ a
singleSequ a = Sequ (\as -> a : as)

(<>) :: Sequ a -> Sequ a -> Sequ a
Sequ x1 <> Sequ x2 = Sequ (\as -> x1 (x2 as))
infixr 5 <>

seqToList :: Sequ a -> [a]
seqToList (Sequ x) = x []

instance Show a => Show (Sequ a) where
    showsPrec d a = showsPrec d (seqToList a)
