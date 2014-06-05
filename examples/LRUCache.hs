{-# LANGUAGE BangPatterns #-}
module Data.LRUCache
  ( LRUCache
  , empty
  , lookup
  , lookupNoLRU
  , insert
  , delete
  , deleteLRU
  ) where


import           Data.Maybe (fromMaybe)
import qualified Data.IntPSQ as IntPSQ

import           Prelude hiding (lookup)

type Tick = Int
type Size = Int

data LRUCache v = LRUCache
    { lrucNextTick :: {-# UNPACK #-} !Tick
    , lrucPsq      ::                !(IntPSQ.IntPSQ Tick v)
    , lrucMaxSize  :: {-# UNPACK #-} !Size
    }

empty :: Int -> LRUCache v
empty maxSize = LRUCache 0 IntPSQ.empty maxSize

size :: LRUCache v -> Int
size = IntPSQ.size . lrucPsq

lookup :: Int -> LRUCache v -> (Maybe v, LRUCache v)
lookup k cache@(LRUCache nextTick psq maxSize) =
    case IntPSQ.alter tickleIfExists k psq of
      (Nothing,       _   ) -> (Nothing, cache)
      (mbV@(Just _),  psq') -> (mbV,     increaseTick nextTick psq' maxSize)
  where
    tickleIfExists Nothing       = (Nothing, Nothing)
    tickleIfExists (Just (_, v)) = (Just v,  Just (nextTick, v))

{-# INLINE lookupNoLRU #-}
lookupNoLRU :: Int -> LRUCache v -> Maybe v
lookupNoLRU k = fmap snd . IntPSQ.lookup k . lrucPsq

increaseTick :: Tick -> IntPSQ.IntPSQ Tick v -> Size -> LRUCache v
increaseTick tick psq maxSize
  | tick < maxBound = LRUCache (tick + 1) psq maxSize
  | otherwise       = retick psq IntPSQ.empty 0
  where
    retick !oldPsq !newPsq !newTick = case IntPSQ.minViewWithKey oldPsq of
      Nothing                   -> LRUCache newTick newPsq maxSize
      Just ((k, _, v), oldPsq') ->
        retick oldPsq' (IntPSQ.insert k newTick v newPsq) (newTick + 1)

insert :: Int -> v -> LRUCache v -> (LRUCache v, Maybe (Int, v))
insert k v (LRUCache nextTick psq maxSize)
    | IntPSQ.size psq' <= maxSize =
        (increaseTick nextTick psq' maxSize, Nothing)
    | otherwise                  =
        fromMaybe (empty maxSize, Nothing) $ do
            ((k, _, v), psq'') <- IntPSQ.minViewWithKey psq'
            return (increaseTick nextTick psq'' maxSize, Just (k, v))
   where
     psq' = IntPSQ.insert k nextTick v psq

-- | Evict the entry at the given key in the cache.
delete :: Int -> LRUCache v -> (LRUCache v, Maybe v)
delete k cache = case IntPSQ.deleteView k (lrucPsq cache) of
    (_,    Nothing)      -> (cache,                  Nothing)
    (psq', Just (_t, v)) -> (cache {lrucPsq = psq'}, Just v)

deleteLRU :: LRUCache v -> (LRUCache v, Maybe (Int, v))
deleteLRU (LRUCache nextTick psq maxSize) =
    fromMaybe (empty maxSize, Nothing) $ do
        ((k, _, v), psq') <- IntPSQ.minViewWithKey psq
        return (LRUCache nextTick psq' maxSize, Just (k, v))


------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------

{- TODO (SM): make validity test work

-- Run a series of consistency checks on the structure inside of the map, return a list of
-- errors if any issues where encountered
valid :: (Eq v, Ord v)
      => LRUCache v -> Maybe String
valid c =
    let w =
         execWriter $ do
             when (size c > lrucMaxSize c)
                 $ tell "Size over the limit\n"
             allTicks <-
               let traverse s minParent maxParent ticks t =
                     case t of
                         Leaf h (L lk lv lt ls) ->
                             (: ticks) <$> checkKey h lk lv lt ls minParent maxParent
                         Collision h ch -> do
                             -- tell "Found collision\n"
                             when (length (unSList ch) < 2) $
                                 tell "Hash collision node with <2 children\n"
                             foldM (\xs (L lk lv lt ls) ->
                                       (: xs) <$> checkKey h lk lv lt ls minParent maxParent
                                   )
                                   ticks
                                   (unSList ch)
                         Node minA maxA minB maxB a b -> do
                             let mint = min minA minB
                                 maxt = max maxA maxB
                             when (s + 1 > bitSize (undefined :: Word)) $
                                 tell "Subkey shift too large during traversal\n"
                             when (mint < minParent || maxt > maxParent) $
                                 tell "Node min/max tick outside of parent interval\n"
                             let used = foldr (\x@(t', _, _) u ->
                                                  case t' of Empty -> u; _ -> x : u
                                              )
                                        []
                                        $ (a, minA, maxA) : (b, minB, maxB) : []
                             when (length used == 0) $
                                 tell "Node with only empty children\n"
                             when (length used == 1) $
                                case (\((x, _, _) : _) -> x) used of
                                    Leaf      _ _ -> tell "Node with single Leaf child\n"
                                    Collision _ _ -> tell "Node with single Collision child\n"
                                    _             -> return ()
                             foldM (\xs (c, mint', maxt') ->
                                       traverse (s + 1) mint' maxt' xs c
                                   )
                                   ticks
                                   used
                         Empty -> return ticks
                   lookupTest v mbV fName =
                       case mbV of
                           Nothing ->
                               tell $ fName ++ ": Can't lookup key found during traversal\n"
                           Just v' ->
                               when (v /= v') $ tell $
                                 fName ++
                                 ": Lookup of key found during traversal yields " ++
                                 "different value\n"
                   checkKey h k v tick s minParent maxParent = do
                       when (H.hash k /= h) $
                           tell "Hash / key mismatch\n"
                       when (tick >= cTick m) $
                           tell "Tick of leaf matches / exceeds current tick\n"
                       when (tick < minParent || tick > maxParent) $
                           tell "Leaf min/max tick outside of parent interval\n"
                       lookupTest v (snd $ lookup k m) "lookup"
                       lookupTest v (lookupNoLRU k m) "lookupNoLRU"
                       let (m', mbV') = delete k m
                       case mbV' of
                         Just (v', s') | v == v' && s == s' ->
                           when (size m' /= (size m - s)) $
                             tell "Deleting key did not reduce size correctly\n"
                         _ ->
                           tell "Delete returned wrong value\n"
                       return tick
               in  traverse 0 minBound maxBound [] $ cTrie m
             when (length allTicks /= numEntries m) $
                 tell "Collection of all tick values used resulted in different numEntries than cSize\n"
             unless (Data.List.null . filter (\x -> length x /= 1) . group . sort $ allTicks) $
                 tell "Duplicate tick value found\n"
             let keysL      = map (^. _1) $ toList m
                 allDeleted = foldl' (\r k -> fst (delete k r)) m keysL
             when (length keysL /= numEntries m) $
                 tell "Length of toList does not match size\n"
             unless (null allDeleted) $
                 tell "Deleting all elements does not result in an empty map\n"
             unless (size allDeleted == 0) $
                 tell "Deleting all elements does not result in a zero size map\n"
             let compacted = compactTicks m
             when ((snd $ popOldest m) /= (snd $ popOldest compacted) ||
                   (snd $ popNewest m) /= (snd $ popNewest compacted)) $
                  tell "Tick compaction changes LRU\n"
             when (sort (toList m) /= sort (toList compacted)) $
                  tell "Tick compaction changes map\n"
             when ((fromIntegral $ cTick compacted) /= numEntries compacted) $
                  tell "Tick compaction did not reduce tick range to minimum\n"
    in case w of
      [] -> Nothing
      xs -> Just xs
-}

{- Make generation of arbitrary test work

arbitraryCache
    :: (Eq k, Hashable k, QC.Arbitrary k, QC.Arbitrary v, Show k, Show v)
    => (v -> Size) -> QC.Gen (Cache k v)
arbitraryCache computeSize = QC.sized $ \n ->
    fromList <$> QC.choose (1, maxSizeLimit)
             <*> sequence [ do k <- QC.arbitrary
                               v <- QC.arbitrary
                               return (k, v, computeSize v)
                          | _ <- [1..n] ]

-- | Newtype with an 'QC.Arbitrary' instance that uses 'NFDataSize' as
-- size-computing function.
newtype NFDataSizeCache k v =
    NFDataSizeCache {unNFDataSizeCache :: Cache k v}

instance (Show k, Show v) => Show (NFDataSizeCache k v) where
    show (NFDataSizeCache cache) = show cache

instance (Eq k, Hashable k, QC.Arbitrary k, NFDataSize v, QC.Arbitrary v, Show k, Show v) =>
         QC.Arbitrary (NFDataSizeCache k v) where
    arbitrary = NFDataSizeCache <$> arbitraryCache rnfSize

-- | A 'Cache' with an arbitrary instance that will most likely evict
-- some objects at creation.
newtype NFDataSizeSmallCache k v =
    NFDataSizeSmallCache {unNFDataSizeSmallCache :: Cache k v}

instance (Show k, Show v) => Show (NFDataSizeSmallCache k v) where
    show (NFDataSizeSmallCache cache) = show cache

instance (Eq k, Hashable k, QC.Arbitrary k, NFDataSize v, QC.Arbitrary v, Show k, Show v) =>
         QC.Arbitrary (NFDataSizeSmallCache k v) where
    arbitrary = QC.sized $ \n -> do
        kvs <- sequence [ do k <- QC.arbitrary
                             v <- QC.arbitrary
                             return (k, v, rnfSize v)
                        | _ <- [1..n] ]
        -- If we don't have any values, we go the normal route.
        maxSize <-
            if n > 0
            then let avgSize = sum [s | (_, _, s) <- kvs] `div` length kvs
                 in  return $ min maxSizeLimit $ max 1 $ avgSize * 3
            else QC.choose (1, maxSizeLimit)
        return $ NFDataSizeSmallCache $ fromList maxSize kvs

-}
