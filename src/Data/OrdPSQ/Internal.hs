{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy         #-}
{-# LANGUAGE BangPatterns        #-}
module Data.OrdPSQ.Internal
    ( -- * Type
      OrdPSQ (..)
    , LTree (..)
    , Elem (..)

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

      -- * Delete/Update
    , delete
    , alter
    , alterMin

      -- * Conversion
    , fromList
    , toList
    , toAscList
    , keys

      -- * Views
    , insertView
    , deleteView
    , minView

      -- * Traversals
    , map
    , fold'

      -- * Tournament view
    , tourView
    , play

      -- * Balancing internals
    , left
    , right
    , maxKey
    , lsingleLeft
    , rsingleLeft
    , lsingleRight
    , rsingleRight
    , ldoubleLeft
    , rdoubleLeft
    , ldoubleRight
    , rdoubleRight
    ) where

import           Prelude         hiding (map, lookup, null, foldr)
import           Control.DeepSeq (NFData(rnf))
import           Data.Maybe      (isJust)
import           Data.Foldable   (Foldable (foldr))

-- | @E k p v@ binds the key @k@ to the value @v@ with priority @p@.
data Elem k p v = E !k !p !v
    deriving (Show)

instance (NFData k, NFData p, NFData v) => NFData (Elem k p v) where
    rnf (E k p v) = rnf k `seq` rnf p `seq` rnf v

unElem :: Elem k p v -> (k, p, v)
unElem (E k p v) = (k, p, v)


------------------------------------------------------------------------
-- | A mapping from keys @k@ to priorites @p@.

data OrdPSQ k p v
    = Void
    | Winner !(Elem k p v)
             !(LTree k p v)
             !k
    deriving (Show)

instance (NFData k, NFData p, NFData v) => NFData (OrdPSQ k p v) where
    rnf Void           = ()
    rnf (Winner e t m) = rnf e `seq` rnf m `seq` rnf t

instance (Ord k, Ord p, Eq v) => Eq (OrdPSQ k p v) where
    x == y = case (minView x, minView y) of
        (Nothing              , Nothing                ) -> True
        (Just (xk, xp, xv, x'), (Just (yk, yp, yv, y'))) ->
            xk == yk && xp == yp && xv == yv && x' == y'
        (Just _               , Nothing                ) -> False
        (Nothing              , Just _                 ) -> False

instance Foldable (OrdPSQ k p) where
    foldr _ z Void                   = z
    foldr f z (Winner (E _ _ x) l _) = f x (foldr f z l)

instance Functor (OrdPSQ k p) where
    fmap f = map (\_ _ v -> f v)

-- | /O(1)/ True if the queue is empty.
null :: OrdPSQ k p v -> Bool
null Void           = True
null (Winner _ _ _) = False

-- | /O(1)/ The number of elements in a queue.
size :: (Ord p) => OrdPSQ k p v -> Int
size Void            = 0
size (Winner _ lt _) = 1 + size' lt

-- | /O(log n)/ Check if a key is present in the the queue.
member :: Ord k => k -> OrdPSQ k p v -> Bool
member k = isJust . lookup k

-- | /O(log n)/ The priority and value of a given key, or Nothing if
-- the key is not bound.
{-# INLINABLE lookup #-}
lookup :: (Ord k) => k -> OrdPSQ k p v -> Maybe (p, v)
lookup k q = tourView
    Nothing
    (\k' p v ->
        if k == k' then Just (p, v) else Nothing)
    (\tl tr ->
        if k <= maxKey tl then lookup k tl else lookup k tr)
    q


------------------------------------------------------------------------
-- Construction

empty :: OrdPSQ k p v
empty = Void

-- | /O(1)/ Build a queue with one element.
singleton :: k -> p -> v -> OrdPSQ k p v
singleton k p v = Winner (E k p v) Start k

------------------------------------------------------------------------
-- Insertion

-- | /O(log n)/ Insert a new key, priority and value in the queue.  If
-- the key is already present in the queue, the associated priority
-- and value are replaced with the supplied priority and value.
{-# INLINABLE insert #-}
insert :: (Ord k, Ord p) => k -> p -> v -> OrdPSQ k p v -> OrdPSQ k p v
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
deleteView :: (Ord k, Ord p) => k -> OrdPSQ k p v -> Maybe (p, v, OrdPSQ k p v)
deleteView k psq = case psq of
    Void            -> Nothing
    Winner (E k' p v) Start _
        | k == k'   -> Just (p, v, empty)
        | otherwise -> Nothing
    Winner e (RLoser _ e' tl m tr) m'
        | k <= m    -> fmap (\(p,v,q) -> (p, v,  q `play` (Winner e' tr m'))) (deleteView k (Winner e tl m))
        | otherwise -> fmap (\(p,v,q) -> (p, v,  (Winner e tl m) `play` q  )) (deleteView k (Winner e' tr m'))
    Winner e (LLoser _ e' tl m tr) m'
        | k <= m    -> fmap (\(p,v,q) -> (p, v, q `play` (Winner e tr m'))) (deleteView k (Winner e' tl m))
        | otherwise -> fmap (\(p,v,q) -> (p, v, (Winner e' tl m) `play` q )) (deleteView k (Winner e tr m'))

{-# INLINABLE insertView #-}
insertView
    :: (Ord k, Ord p)
    => k -> p -> v -> OrdPSQ k p v -> (Maybe (p, v), OrdPSQ k p v)
insertView k p x t = case deleteView k t of
    Nothing          -> (Nothing,       insert k p x t)
    Just (p', x', _) -> (Just (p', x'), insert k p x t)


------------------------------------------------------------------------
-- Delete/Update

-- | /O(log n)/ Delete a key and its priority and value from the
-- queue.  When the key is not a member of the queue, the original
-- queue is returned.
{-# INLINABLE delete #-}
delete :: (Ord k, Ord p) => k -> OrdPSQ k p v -> OrdPSQ k p v
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

{-# INLINE alter #-}
alter :: (Ord k, Ord p)
      => (Maybe (p, v) -> (b, Maybe (p, v)))
      -> k
      -> OrdPSQ k p v
      -> (b, OrdPSQ k p v)
alter f k psq0 =
    let (psq1, mbPV) = case deleteView k psq0 of
                         Nothing          -> (psq0, Nothing)
                         Just (p, v, psq) -> (psq, Just (p, v))
        (!b, mbPV') = f mbPV
    in case mbPV' of
         Nothing     -> (b, psq1)
         Just (p, v) -> (b, insert k p v psq1)

{-# INLINE alterMin #-}
alterMin :: (Ord k, Ord p)
         => (Maybe (k, p, v) -> (b, Maybe (k, p, v)))
         -> OrdPSQ k p v
         -> (b, OrdPSQ k p v)
alterMin f psq0 =
    case minView psq0 of
        Nothing -> let (!b, mbKPV) = f Nothing
                   in (b, insertMay mbKPV psq0)
        Just (k,p,v, psq1) -> let (!b, mbKPV) = f $ Just (k, p, v)
                              in (b, insertMay mbKPV psq1)
  where
    insertMay Nothing          psq = psq
    insertMay (Just (k, p, v)) psq = insert k p v psq

------------------------------------------------------------------------
-- Conversion

-- | /O(n*log n)/ Build a queue from a list of key/priority/value
-- tuples.  If the list contains more than one priority and value for
-- the same key, the last priority and value for the key is retained.
{-# INLINABLE fromList #-}
fromList :: (Ord k, Ord p) => [(k, p, v)] -> OrdPSQ k p v
fromList = foldr (\(k, p, v) q -> insert k p v q) empty

-- | /O(n)/ Convert to a list of key/priority/value tuples.
toList :: OrdPSQ k p v -> [(k, p, v)]
toList = toAscList

-- | /O(n)/ Obtain the list of present keys in the queue.
keys :: OrdPSQ k p v -> [k]
keys t = [k | (k, _, _) <- toList t]
-- TODO (jaspervdj): There must be faster implementations.

-- | /O(n)/ Convert to an ascending list.
toAscList :: OrdPSQ k p v -> [(k, p, v)]
toAscList q  = seqToList (toAscLists q)

toAscLists :: OrdPSQ k p v -> Sequ (k, p, v)
toAscLists = tourView
    emptySequ
    (\k p v -> singleSequ (k, p, v))
    (\tl tr -> toAscLists tl <> toAscLists tr)


------------------------------------------------------------------------
-- Traversals

{-# INLINABLE map #-}
map :: forall k p v w. (k -> p -> v -> w) -> OrdPSQ k p v -> OrdPSQ k p w
map f =
    goPSQ
  where
    goPSQ :: OrdPSQ k p v -> OrdPSQ k p w
    goPSQ Void           = Void
    goPSQ (Winner e l k) = Winner (goElem e) (goLTree l) k

    goElem :: Elem k p v -> Elem k p w
    goElem (E k p x) = E k p (f k p x)

    goLTree :: LTree k p v -> LTree k p w
    goLTree Start              = Start
    goLTree (LLoser s e l k r) = LLoser s (goElem e) (goLTree l) k (goLTree r)
    goLTree (RLoser s e l k r) = RLoser s (goElem e) (goLTree l) k (goLTree r)


{-# INLINE fold' #-}
fold' :: (k -> p -> v -> a -> a) -> a -> OrdPSQ k p v -> a
fold' f =
    \acc0 psq -> case psq of
                   Void                   -> acc0
                   (Winner (E k p v) t _) ->
                        let !acc1 = f k p v acc0
                        in  go acc1 t
  where
    go !acc Start                        = acc
    go !acc (LLoser _ (E k p v) lt _ rt) = go (f k p v (go acc lt)) rt
    go !acc (RLoser _ (E k p v) lt _ rt) = go (f k p v (go acc lt)) rt


------------------------------------------------------------------------
-- Min

-- | /O(1)/ The element with the lowest priority.
findMin :: OrdPSQ k p v -> Maybe (k, p, v)
findMin Void           = Nothing
findMin (Winner e _ _) = Just (unElem e)

-- | /O(log n)/ Retrieve the binding with the least priority, and the
-- rest of the queue stripped of that binding.
{-# INLINABLE minView #-}
minView :: (Ord k, Ord p) => OrdPSQ k p v -> Maybe (k, p, v, OrdPSQ k p v)
minView Void                   = Nothing
minView (Winner (E k p v) t m) = Just (k, p, v, secondBest t m)

{-# INLINABLE secondBest #-}
secondBest :: (Ord k, Ord p) => LTree k p v -> k -> OrdPSQ k p v
secondBest Start _                 = Void
secondBest (LLoser _ e tl m tr) m' = Winner e tl m `play` secondBest tr m'
secondBest (RLoser _ e tl m tr) m' = secondBest tl m `play` Winner e tr m'


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
    deriving (Show)

instance (NFData k, NFData p, NFData v) => NFData (LTree k p v) where
    rnf Start              = ()
    rnf (LLoser _ e l k r) = rnf e `seq` rnf l `seq` rnf k `seq` rnf r
    rnf (RLoser _ e l k r) = rnf e `seq` rnf l `seq` rnf k `seq` rnf r

instance Foldable (LTree k p) where
    foldr _ z Start                      = z
    foldr f z (LLoser _ (E _ _ x) l _ r) = f x (foldr f (foldr f z r) l)
    foldr f z (RLoser _ (E _ _ x) l _ r) = f x (foldr f (foldr f z r) l)

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

maxKey :: OrdPSQ k p v -> k
maxKey Void           = moduleError "maxKey" "empty queue"
maxKey (Winner _ _ m) = m

lloser, rloser :: k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
lloser k p v tl m tr = LLoser (1 + size' tl + size' tr) (E k p v) tl m tr
rloser k p v tl m tr = RLoser (1 + size' tl + size' tr) (E k p v) tl m tr

------------------------------------------------------------------------
-- Balancing

-- | Balance factor
omega :: Int
omega = 4  -- Has to be greater than 3.75 because Hinze's paper said so.

lbalance, rbalance
    :: (Ord k, Ord p)
    => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
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

lbalanceLeft
    :: (Ord k, Ord p)
    => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
lbalanceLeft  k p v l m r
    | size' (left r) < size' (right r) = lsingleLeft  k p v l m r
    | otherwise                        = ldoubleLeft  k p v l m r

lbalanceRight
    :: (Ord k, Ord p)
    => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
lbalanceRight k p v l m r
    | size' (left l) > size' (right l) = lsingleRight k p v l m r
    | otherwise                        = ldoubleRight k p v l m r

rbalanceLeft
    :: (Ord k, Ord p)
    => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
rbalanceLeft  k p v l m r
    | size' (left r) < size' (right r) = rsingleLeft  k p v l m r
    | otherwise                        = rdoubleLeft  k p v l m r

rbalanceRight
    :: (Ord k, Ord p)
    => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
rbalanceRight k p v l m r
    | size' (left l) > size' (right l) = rsingleRight k p v l m r
    | otherwise                        = rdoubleRight k p v l m r

lsingleLeft
    :: (Ord k, Ord p)
    => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
lsingleLeft k1 p1 v1 t1 m1 (LLoser _ (E k2 p2 v2) t2 m2 t3)
    | (p1, k1) `beats` (p2, k2) =
        lloser k1 p1 v1 (rloser k2 p2 v2 t1 m1 t2) m2 t3
    | otherwise                 =
        lloser k2 p2 v2 (lloser k1 p1 v1 t1 m1 t2) m2 t3
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

rsingleRight
    :: (Ord k, Ord p)
    => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
rsingleRight k1 p1 v1 (LLoser _ (E k2 p2 v2) t1 m1 t2) m2 t3 =
    lloser k2 p2 v2 t1 m1 (rloser k1 p1 v1 t2 m2 t3)
rsingleRight k1 p1 v1 (RLoser _ (E k2 p2 v2) t1 m1 t2) m2 t3
    | (p1, k1) `beats` (p2, k2) =
        rloser k1 p1 v1 t1 m1 (lloser k2 p2 v2 t2 m2 t3)
    | otherwise                 =
        rloser k2 p2 v2 t1 m1 (rloser k1 p1 v1 t2 m2 t3)
rsingleRight _ _ _ _ _ _ = moduleError "rsingleRight" "malformed tree"

ldoubleLeft
    :: (Ord k, Ord p)
    => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
ldoubleLeft k1 p1 v1 t1 m1 (LLoser _ (E k2 p2 v2) t2 m2 t3) =
    lsingleLeft k1 p1 v1 t1 m1 (lsingleRight k2 p2 v2 t2 m2 t3)
ldoubleLeft k1 p1 v1 t1 m1 (RLoser _ (E k2 p2 v2) t2 m2 t3) =
    lsingleLeft k1 p1 v1 t1 m1 (rsingleRight k2 p2 v2 t2 m2 t3)
ldoubleLeft _ _ _ _ _ _ = moduleError "ldoubleLeft" "malformed tree"

ldoubleRight
    :: (Ord k, Ord p)
    => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
ldoubleRight k1 p1 v1 (LLoser _ (E k2 p2 v2) t1 m1 t2) m2 t3 =
    lsingleRight k1 p1 v1 (lsingleLeft k2 p2 v2 t1 m1 t2) m2 t3
ldoubleRight k1 p1 v1 (RLoser _ (E k2 p2 v2) t1 m1 t2) m2 t3 =
    lsingleRight k1 p1 v1 (rsingleLeft k2 p2 v2 t1 m1 t2) m2 t3
ldoubleRight _ _ _ _ _ _ = moduleError "ldoubleRight" "malformed tree"

rdoubleLeft
    :: (Ord k, Ord p)
    => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
rdoubleLeft k1 p1 v1 t1 m1 (LLoser _ (E k2 p2 v2) t2 m2 t3) =
    rsingleLeft k1 p1 v1 t1 m1 (lsingleRight k2 p2 v2 t2 m2 t3)
rdoubleLeft k1 p1 v1 t1 m1 (RLoser _ (E k2 p2 v2) t2 m2 t3) =
    rsingleLeft k1 p1 v1 t1 m1 (rsingleRight k2 p2 v2 t2 m2 t3)
rdoubleLeft _ _ _ _ _ _ = moduleError "rdoubleLeft" "malformed tree"

rdoubleRight
    :: (Ord k, Ord p)
    => k -> p -> v -> LTree k p v -> k -> LTree k p v -> LTree k p v
rdoubleRight k1 p1 v1 (LLoser _ (E k2 p2 v2) t1 m1 t2) m2 t3 =
    rsingleRight k1 p1 v1 (lsingleLeft k2 p2 v2 t1 m1 t2) m2 t3
rdoubleRight k1 p1 v1 (RLoser _ (E k2 p2 v2) t1 m1 t2) m2 t3 =
    rsingleRight k1 p1 v1 (rsingleLeft k2 p2 v2 t1 m1 t2) m2 t3
rdoubleRight _ _ _ _ _ _ = moduleError "rdoubleRight" "malformed tree"

-- | Take two pennants and returns a new pennant that is the union of
-- the two with the precondition that the keys in the ﬁrst tree are
-- strictly smaller than the keys in the second tree.
{-# INLINABLE play #-}
play :: (Ord p, Ord k) => OrdPSQ k p v -> OrdPSQ k p v -> OrdPSQ k p v
Void `play` t' = t'
t `play` Void  = t
Winner e@(E k p v) t m `play` Winner e'@(E k' p' v') t' m'
    | (p, k) `beats` (p', k') = Winner e (rbalance k' p' v' t m t') m'
    | otherwise               = Winner e' (lbalance k p v t m t') m'

-- | When priorities are equal, the tree with the lowest key wins. This is
-- important to have a deterministic `==`, which requires on `minView` pulling
-- out the elements in the right order.
beats :: (Ord p, Ord k) => (p, k) -> (p, k) -> Bool
beats (p, !k) (p', !k') = p < p' || (p == p' && k <= k')
{-# INLINE beats #-}

-- | Obtain a "Tournament view" of the OrdPSQ. This is a tree represented by
-- three constructors:
--
-- * An empty tree
--
-- * A singleton tree
--
-- * Two non-empty trees playing against each other in the tournament.
--
-- We use a church encoding for the constructors as an optimisation.
tourView
    :: r                                    -- ^ Empty constructor
    -> (k -> p -> v -> r)                   -- ^ Singleton constructor
    -> (OrdPSQ k p v -> OrdPSQ k p v -> r)  -- ^ Branch/play constructor
    -> OrdPSQ k p v                         -- ^ OrdPSQ to view
    -> r                                    -- ^ Result
tourView void single branch q = case q of
    Void                                -> void
    (Winner (E k p x) Start _)       -> single k p x
    (Winner e (RLoser _ e' tl m tr) m') ->
        Winner e tl m `branch` Winner e' tr m'
    (Winner e (LLoser _ e' tl m tr) m') ->
        Winner e' tl m `branch` Winner e tr m'
{-# INLINE tourView #-}


------------------------------------------------------------------------
-- Utility functions

moduleError :: String -> String -> a
moduleError fun msg = error ("Data.OrdPSQ.Internal." ++ fun ++ ':' : ' ' : msg)
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
