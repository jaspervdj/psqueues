{-# LANGUAGE BangPatterns, UnboxedTuples #-}
{-# OPTIONS_GHC -fobject-code #-}
-- | Originally written by Thomas Schilling based on R. Hinze's paper.
module Data.PSQ
  ( PSQ
  , Key, Prio, ValueSize
  , empty
  , singleton
  , insert
  , delete
  , fromList
  , size

  , lookup
  , lookup'
  , lookupMinPrio

  , adjustPrio
  , lookupAdjustPrio

  , viewMin
  , toAscList
  , toPriorityList

  -- , ex1, ex2, ex3
  -- , renderPSQ
  )
where

import           Prelude hiding (lookup)
-- import           Text.PrettyPrint hiding (empty)
import           Data.List (foldl')
--import qualified Test.QuickCheck.Arbitrary as QC

--import Debug.Trace

------------------------------------------------------------------------------

type Key       = Int
type Prio      = Int
type ValueSize = Int
type TreeSize  = Int

-- | A priority search queue with values of type @a@.
--
-- Items are ordered by @(Key, k)@, i.e., the full key is only used to resolve
-- conflicts between items that hash to the same hash key.
data PSQ k a
  = Void
  | Winner {-# UNPACK #-} !Key   -- hash of key of lowest-prio element
                          !k     -- full key of lowest-prio element
           {-# UNPACK #-} !Prio  -- priority of lowest-prio element
                          !a     -- value of lowest-prio element
           {-# UNPACK #-} !ValueSize  -- cached size of value
                          !(BalTree k a) -- all other nodes
           {-# UNPACK #-} !Key   -- maximum key in balanced tree
                          !k

-- | A balanced tree
data BalTree k a
  = Leaf
  | Node {-# UNPACK #-} !TreeSize
         {-# UNPACK #-} !Key
                        !k
         {-# UNPACK #-} !Prio
                        !a
         {-# UNPACK #-} !ValueSize
                        !(BalTree k a) -- left subtree
         {-# UNPACK #-} !Key         -- all keys in left subtree must be <= than
                                     -- this key
                        !k
                        !(BalTree k a) -- right subtree

-- | Create an empty 'PSQ'.
empty :: PSQ k a
empty = Void

-- | Create a 'PSQ' with a single element.
singleton :: Key -> k -> Prio -> a -> ValueSize -> PSQ k a
singleton key fkey prio v vSize =
  Winner key fkey prio v vSize leaf key fkey
{-# INLINE singleton #-}

-- | Internal operation for merging two 'PSQ's.
--
-- Precondition: All the keys in the left argument must be strictly less than
-- the keys in the right argument.
merge :: Ord k => PSQ k a -> PSQ k a -> PSQ k a
merge Void p = p
merge p Void = p
merge (Winner key1 fkey1 prio1 v1 vSize1 t1 keyM1 fkeyM1)
      (Winner key2 fkey2 prio2 v2 vSize2 t2 keyM2 fkeyM2)
  | prio1 <= prio2
  = Winner key1 fkey1 prio1 v1 vSize1
           (node key2 fkey2 prio2 v2 vSize2 t1 keyM1 fkeyM1 t2) keyM2 fkeyM2
  | otherwise
  = Winner key2 fkey2 prio2 v2 vSize2
           (node key1 fkey1 prio1 v1 vSize1 t1 keyM1 fkeyM1 t2) keyM2 fkeyM2

{-# INLINE leKey #-}
leKey :: Ord k => Key -> k -> Key -> k -> Bool
leKey k1 fk1 k2 fk2
  | k1 < k2 = True
  | k1 > k2 = False
  | otherwise = fk1 <= fk2

{-# INLINE eqKey #-}
eqKey :: Eq k => Key -> k -> Key -> k -> Bool
eqKey k1 fk1 k2 fk2 = k1 == k2 && fk1 == fk2

toAscList :: Ord k => PSQ k a -> [(Key, k, Prio, a, ValueSize)]
toAscList Void = []
toAscList (Winner key0 fkey0 prio0 v0 vSize0 t _ _) =
  go key0 fkey0 prio0 v0 vSize0 t []
 where
   go key fkey prio v vSize Leaf kont
     = (key, fkey, prio, v, vSize) : kont
   go key fkey prio v vSize
      (Node _ key1 fkey1 prio1 v1 vSize1 tl keyM1 fkeyM1 tr) kont
     | leKey key1 fkey1 keyM1 fkeyM1
     = go key1 fkey1 prio1 v1 vSize1 tl (go key fkey prio v vSize tr kont)
     | otherwise
     = go key fkey prio v vSize tl (go key1 fkey1 prio1 v1 vSize1 tr kont)

instance (Ord k, Show k, Show a) => Show (PSQ k a) where
  show p = "fromList " ++ show (toAscList p)

leMaxKey :: Ord k => Key -> k -> PSQ k a -> Bool
leMaxKey key fkey (Winner _key _fkey _prio _v _vSize _t keyM fkeyM) =
  leKey key fkey keyM fkeyM
leMaxKey _ _ Void = error "maxKey of empty PSQ"

-- maxKey :: PSQ a -> Key
-- maxKey (Winner _key _prio _v _vSize _t m) = m
-- maxKey Void

size :: PSQ k a -> Int
size Void = 0
size (Winner _ _ _ _ _ Leaf _ _) = 1
size (Winner _ _ _ _ _ (Node s _ _ _ _ _ _ _ _ _) _ _) = s + 1

------------------------------------------------------------------------------
-- Views

data ViewPSQ k a
  = VNil
  | VSingleton !Key !k !Prio !a !ValueSize
  | VMerge (PSQ k a) (PSQ k a)

viewPSQ :: Ord k => PSQ k a -> ViewPSQ k a
viewPSQ Void = VNil
viewPSQ (Winner key fkey prio v vSize Leaf _keyM _fkeyM)
  = VSingleton key fkey prio v vSize
viewPSQ (Winner key fkey prio v vSize
                (Node _ key1 fkey1 prio1 v1 vSize1 tl keyM1 fkeyM1 tr) keyM fkeyM)
  | leKey key1 fkey1 keyM1 fkeyM1
  = VMerge (Winner key1 fkey1 prio1 v1 vSize1 tl keyM1 fkeyM1)
           (Winner key fkey prio v vSize tr keyM fkeyM)
  | otherwise
  = VMerge (Winner key fkey prio v vSize tl keyM1 fkeyM1)
           (Winner key1 fkey1 prio1 v1 vSize1 tr keyM fkeyM)
{-# INLINE viewPSQ #-}

viewMin :: Ord k => PSQ k a -> Maybe (Key, k, Prio, a, ValueSize, PSQ k a)
viewMin Void = Nothing
viewMin (Winner key fkey prio v vSize t keyM0 fkeyM0) =
  Just (key, fkey, prio, v, vSize, secondBest keyM0 fkeyM0 t)
 where
   secondBest _keyM _fkeyM Leaf
     = Void
   secondBest keyM fkeyM (Node _ key1 fkey1 prio1 v1 vSize1 t1 keyM1 fkeyM1 t2)
     | leKey key1 fkey1 keyM1 fkeyM1
     = Winner key1 fkey1 prio1 v1 vSize1 t1 keyM1 fkeyM1 `merge`
       secondBest keyM fkeyM t2
     | otherwise
     = secondBest keyM1 fkeyM1 t1 `merge`
       Winner key1 fkey1 prio1 v1 vSize1 t2 keyM fkeyM

lookupMinPrio :: PSQ k a -> Maybe Prio
lookupMinPrio Void = Nothing
lookupMinPrio (Winner _key _fkey prio _v _vSize _t _keyM _fkeyM) = Just prio

toPriorityList :: Ord k => PSQ k a -> [(Key, k, Prio, a, ValueSize)]
toPriorityList psq = case viewMin psq of
  Nothing -> []
  Just (key, fkey, prio, v, vSize, psq') ->
    (key, fkey, prio, v, vSize) : toPriorityList psq'

------------------------------------------------------------------------------

fromList :: Ord k => [(Key, k, Prio, a, ValueSize)] -> PSQ k a
fromList = foldl' ins1 empty
 where
   ins1 psq (key, fkey, prio, v, vSize) = insert key fkey prio v vSize psq

insert :: Ord k => Key -> k -> Prio -> a -> ValueSize -> PSQ k a -> PSQ k a
insert !key !fkey !prio !v !vSize p =
  case viewPSQ p of
    VNil -> singleton key fkey prio v vSize
    VSingleton key1 fkey1 prio1 v1 vSize1
      | eqKey key fkey key1 fkey1
      -> singleton key fkey prio v vSize  -- update

      | leKey key fkey key1 fkey1
      -> singleton key fkey prio v vSize `merge`
         singleton key1 fkey1 prio1 v1 vSize1

      | otherwise
      -> singleton key1 fkey1 prio1 v1 vSize1 `merge`
         singleton key fkey prio v vSize

    VMerge pl pr
      | leMaxKey key fkey pl
      -> insert key fkey prio v vSize pl `merge` pr
      | otherwise
      -> pl `merge` insert key fkey prio v vSize pr

-- | Look up the value associated with the given key.
lookup :: Ord k => Key -> k -> PSQ k a -> Maybe (Prio, a)
lookup key fkey psq = case viewPSQ psq of
  VNil -> Nothing
  VSingleton key1 fkey1 prio1 v1 _vSize1
   | eqKey key fkey key1 fkey1 -> Just (prio1, v1)
   | otherwise                 -> Nothing
  VMerge pl pr
   | leMaxKey key fkey pl -> lookup key fkey pl
   | otherwise            -> lookup key fkey pr

lookup' :: Ord k => Key -> k -> PSQ k a -> Maybe (Prio, a)
lookup' _key _fkey Void = Nothing
lookup' kkey fkey (Winner key0 fkey0 prio0 v0 _vSize0 t _keyM0 _fkeyM0)
  | eqKey kkey fkey key0 fkey0 = Just (prio0, v0)
  | otherwise                  = go kkey t
 where
   --go key n | trace (show (key, renderBalTree (\_ -> text "?") n)) False = undefined
   go _key Leaf = Nothing
   go key (Node _tsz key1 fkey1 prio1 v1 _vSize1 tl keyM fkeyM tr)
     | eqKey key fkey key1 fkey1 = Just (prio1, v1)
     | leKey key fkey keyM fkeyM = go key tl
     | otherwise                 = go key tr

adjustPrio :: Ord k => Key -> k -> (Prio -> Prio) -> PSQ k a -> PSQ k a
adjustPrio !kkey fkey_ f psq = case viewPSQ psq of
  VNil -> psq
  VSingleton key fkey prio v vSize
   | eqKey kkey fkey_ key fkey -> singleton key fkey (f prio) v vSize
   | otherwise                 -> psq
  VMerge pl pr
   | leMaxKey kkey fkey_ pl -> adjustPrio kkey fkey_ f pl `merge` pr
   | otherwise              -> pl `merge` adjustPrio kkey fkey_ f pr

delete :: Ord k => Key -> k -> PSQ k a -> PSQ k a
delete !kkey kfkey psq = case viewPSQ psq of
  VNil -> psq
  VSingleton key fkey _prio _v _vSize
   | eqKey kkey kfkey key fkey -> empty
   | otherwise                 -> psq
  VMerge pl pr
   | leMaxKey kkey kfkey pl -> delete kkey kfkey pl `merge` pr
   | otherwise              -> pl `merge` delete kkey kfkey pr

--data STup a b c = STup !a !b !c

lookupAdjustPrio :: Ord k => Key -> k -> (Prio -> Prio) -> PSQ k a
                 -> (Maybe a, Maybe (PSQ k a))
                -- ^ The item associated with key and a modified PSQ if any
                -- changes were made.
lookupAdjustPrio !kkey_ !kfkey_ f_ psq_ =
   let (!r, !changed, !psq' ) = go kkey_ kfkey_ f_ psq_ in
   if changed then (r, Just psq') else (r, Nothing)
 where
   go :: Ord k => Key -> k -> (Prio -> Prio) -> PSQ k a
      -> ( Maybe a, Bool, PSQ k a )
   -- returns: the lookup result, whether any modifications were performed,
   -- the adjusted tree.  If the second argument is @False@, then it's
   -- guaranteed to be equal to the input PSQ.
   go kkey kfkey f psq = case viewPSQ psq of
     VNil -> ( Nothing, False, psq )
     VSingleton key fkey prio v vSize
      | eqKey kkey kfkey key fkey
      -> let !prio' = f prio in
         if prio' == prio then ( Just v, False, psq )
                          else ( Just v, True, singleton key fkey prio' v vSize )
      | otherwise
      -> ( Nothing, False, psq )
     VMerge pl pr
      | leMaxKey kkey kfkey pl
      -> let ( !r, !changed, !pl' ) = go kkey kfkey f pl in
         if changed then ( r, True,  pl' `merge` pr )
                    else ( r, False, psq )
      | otherwise
      -> let ( !r, !changed, !pr' ) = go kkey kfkey f pr in
         if changed then ( r, True, pl `merge` pr' )
                    else ( r, False, psq )


{-
insert key prio v vSize p@(Winner key1 _prio1 _v1 _vSize1 Leaf _keyM1)
  | key < key1  = singleton key prio v vSize `merge` p
  | key == key1 = singleton key prio v vSize -- update
  | otherwise   = p `merge` singleton key prio v vSize
insert key prio v vSize p@(Winner key1 _prio1 _v1 _vSize1 t1 _keyM1)
-}
------------------------------------------------------------------------------

{-
-- | Render the internal structure of the PSQ as a tree.
renderPSQ :: (k -> Doc) -> (a -> Doc) -> PSQ k a -> Doc
renderPSQ _ _ Void = text "void"
renderPSQ pk pa (Winner key fkey prio v vSize t keyM fkeyM) =
  text "Winner[k=" <> int key <> comma <> pk fkey <>
  text ",p=" <> int prio <>
  text ",v=" <> pa v <>
  text ",sz=" <> int vSize <>
  text ",km=" <> int keyM <> comma <> pk fkeyM <> char ']' $$
  text ".." <> renderBalTree pk pa t

renderBalTree :: (k -> Doc) -> (a -> Doc) -> BalTree k a -> Doc
renderBalTree _ _ Leaf = text "leaf"
renderBalTree pk pa (Node tsz key fkey prio v vSize t1 keyM fkeyM t2) =
  text "Node[k=" <> int key <> comma <> pk fkey <>
  text ",km=" <> int keyM <> comma <> pk fkeyM <>
  text ",p=" <> int prio <>
  text ",v=" <> pa v <>
  text ",sz=" <> int vSize <>
  text ",w=" <> int tsz <> char ']' $$
  vcat [text ".." <> renderBalTree pk pa t1,
        text ".." <> renderBalTree pk pa t2]

-}

{-
renderEx :: PSQ String -> Doc
renderEx = renderPSQ (text . show)

ex1 :: PSQ String
ex1 = empty

ex2 :: PSQ String
ex2 = merge (singleton 5 2 "abc" 3)
            (singleton 24 1 "def" 3)

ex3 :: PSQ String
ex3 = foldr ins empty dats
 where
   ins (k, p, s) psq = insert k p s (length s) psq
   dats = [ (2,  6, "foo")
          , (21, 3, "fooa")
          , (24, 9, "xaz")
          , (11, 8, "mea")
          , (7,  8, "fooueo")
          , (23, 1, "bar")
          , (9, 10, "eoame")
          , (43, 7, "fooa")
          , (1,  4, "xa")
          , (12, 5, "y")
          , (8,  2, "zz")
          ]

test1 = do
  let p = ex3
  mapM_ print $
    [ (x, y == lookup' x p, y, lookup' x p)
    | (x, _, _) <- toAscList p
    , let y = lookup x p
    ]
-}
-- lookup :: Key -> PSQ a -> Maybe (a, Prio, ValueSize)
-- lookup key (

------------------------------------------------------------------------------
-- Weight Balanced Trees
------------------------------------------------------------------------------

-- A tree is weight-balanced if for all nodes, *either*
--
--  - both subtrees have at most one element, *or*
--  - one subtree does not have more than omega times as many elements as the
--    opposite subtree, where omega is some constant > 3.75

-- | The
omega :: TreeSize
omega = 4

leaf :: BalTree k a
leaf = Leaf

bsize :: BalTree k a -> TreeSize
bsize Leaf                       = 0
bsize (Node s _ _ _ _ _ _ _ _ _) = s

nodeUnbal :: Key -> k -> Prio -> a -> ValueSize
          -> BalTree k a -> Key -> k -> BalTree k a
          -> BalTree k a
nodeUnbal key fkey prio v vSize l k fk r =
  Node (1 + bsize l + bsize r) key fkey prio v vSize l k fk r
{-# INLINE nodeUnbal #-}

-- | Smart constructor for tree node which performs automatic balancing.
node :: Ord k =>
        Key -> k -> Prio -> a -> ValueSize
     -> BalTree k a -> Key -> k -> BalTree k a
     -> BalTree k a
node !key fkey !prio !v !vSize !l !k !fk !r
  | bsize l + bsize r < 2     = nodeUnbal key fkey prio v vSize l k fk r
  | bsize r > omega * bsize l = balanceLeft key fkey prio v vSize l k fk r
  | bsize l > omega * bsize r = balanceRight key fkey prio v vSize l k fk r
  | otherwise                 = nodeUnbal key fkey prio v vSize l k fk r

balanceLeft :: Ord k => Key -> k -> Prio -> a -> ValueSize
            -> BalTree k a -> Key -> k -> BalTree k a
            -> BalTree k a
balanceLeft !key !fkey !prio !v !vSize !l !k !fk
            !r@(Node _sz _key _fkey _prio _v _vSize rl _ _ rr)
  | bsize rl < bsize rr = singleLeft key fkey prio v vSize l k fk r
  | otherwise         = doubleLeft key fkey prio v vSize l k fk r
balanceLeft !_key !_fkey !_prio !_v !_vSize !_l !_k !_fk Leaf =
  error $ "INVARIANT balanceLeft: right subtree cannot be Leaf"
{-# INLINE balanceLeft #-}

balanceRight :: Ord k => Key -> k -> Prio -> a -> ValueSize
             -> BalTree k a -> Key -> k -> BalTree k a
             -> BalTree k a
balanceRight !key !fkey !prio !v !vSize
             !l@(Node _sz _key _fkey _prio _v _vSize ll _ _ lr)
             !k !fk !r
  | bsize lr < bsize ll = singleRight key fkey prio v vSize l k fk r
  | otherwise           = doubleRight key fkey prio v vSize l k fk r
balanceRight !_key !_fkey !_prio !_v !_vSize Leaf !_k !_fk !_r =
  error $ "INVARIANT balanceRight: left subtree cannot be Leaf"
{-# INLINE balanceRight #-}

singleLeft :: Ord k =>
              Key -> k -> Prio -> a -> ValueSize
           -> BalTree k a -> Key -> k -> BalTree k a
           -> BalTree k a
singleLeft !key1 !fkey1 !prio1 !v1 !vSize1 !t1 !keyM1 !fkeyM1
           (Node _sz key2 fkey2 prio2 v2 vSize2 t2 keyM2 fkeyM2 t3)
  | leKey key2 fkey2 keyM2 fkeyM2 && prio1 <= prio2
  = nodeUnbal key1 fkey1 prio1 v1 vSize1
              (nodeUnbal key2 fkey2 prio2 v2 vSize2 t1 keyM1 fkeyM1 t2)
              keyM2 fkeyM2 t3
  | otherwise
  = nodeUnbal key2 fkey2 prio2 v2 vSize2
              (nodeUnbal key1 fkey1 prio1 v1 vSize1 t1 keyM1 fkeyM1 t2)
              keyM2 fkeyM2 t3
singleLeft !_key1 !_fkey1 !_prio1 !_v1 !_vSize1 !_t1 !_keyM1 !_ Leaf =
  error $ "INVARIANT singleLeft: right subtree cannot be Leaf"

singleRight :: Ord k => Key -> k -> Prio -> a -> ValueSize
            -> BalTree k a -> Key -> k -> BalTree k a
            -> BalTree k a
singleRight !key1 !fkey1 !prio1 !v1 !vSize1
            (Node _sz key2 fkey2 prio2 v2 vSize2 t1 keyM1 fkeyM1 t2)
            !keyM2 !fkeyM2 t3
  | not (leKey key2 fkey2 keyM1 fkeyM1) && prio1 <= prio2
  = nodeUnbal key1 fkey1 prio1 v1 vSize1 t1 keyM1 fkeyM1
              (nodeUnbal key2 fkey2 prio2 v2 vSize2 t2 keyM2 fkeyM2 t3)
  | otherwise
  = nodeUnbal key2 fkey2 prio2 v2 vSize2 t1 keyM1 fkeyM1
              (nodeUnbal key1 fkey1 prio1 v1 vSize1 t2 keyM2 fkeyM2 t3)
singleRight !_key1 !_fkey1 !_prio1 !_v1 !_vSize1 Leaf !_keyM1 !_ !_t3 =
  error $ "INVARIANT singleRight: left subtree cannot be Leaf"

doubleLeft :: Ord k => Key -> k -> Prio -> a -> ValueSize
           -> BalTree k a -> Key -> k -> BalTree k a
           -> BalTree k a
doubleLeft !key1 fkey1 !prio1 !v1 !vSize1 !t1 !keyM1 !fkeyM1
           (Node _sz key2 fkey2 prio2 v2 vSize2 t2 keyM2 fkeyM2 t3)
  = singleLeft key1 fkey1 prio1 v1 vSize1 t1 keyM1 fkeyM1
               (singleRight key2 fkey2 prio2 v2 vSize2 t2 keyM2 fkeyM2 t3)
doubleLeft !_key1 !_fkey1 !_prio1 !_v1 !_vSize1 !_t1 !_keyM1 !_fkeyM1 Leaf =
  error $ "INVARIANT doubleLeft: right subtree cannot be Leaf"

doubleRight :: Ord k => Key -> k -> Prio -> a -> ValueSize
            -> BalTree k a -> Key -> k -> BalTree k a
            -> BalTree k a
doubleRight !key1 !fkey1 !prio1 !v1 !vSize1
            (Node _sz key2 fkey2 prio2 v2 vSize2 t1 keyM1 fkeyM1 t2)
            !keyM2 !fkeyM2 t3
  = singleRight key1 fkey1 prio1 v1 vSize1
                (singleLeft key2 fkey2 prio2 v2 vSize2 t1 keyM1 fkeyM1 t2)
                keyM2 fkeyM2 t3
doubleRight !_key1 !_fkey1 !_prio1 !_v1 !_vSize1 Leaf !_keyM1 _ !_t3 =
  error $ "INVARIANT doubleRight: left subtree cannot be Leaf"

------------------------------------------------------------------------------
