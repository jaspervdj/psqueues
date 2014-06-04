module Data.LRUCache where


import qualified Data.IntPSQ as IntPSQ

type Tick = Int
type Size = Int

data LRUCache v = LRUCache
    { lrucNextTick :: {-# UNPACK #-} !Tick
    , lrucPsq      ::                !IntPSQ Tick v
    , lrucMaxSize  :: {-# UNPACK #-} !Size
    }

lookup :: Int -> LRUCache v -> (Maybe v, LRUCache v)
lookup k cache@(LRUCache nextTick psq maxSize) =
    case IntPSQ.alter tickleIfExists k psq of
      (Nothing,       _   ) -> (Nothing, cache)
      (mbV@(Just _),  psq') -> (mbV,     increaseTick nextTick psq' maxSize)
  where
    tickleIfExists Nothing       = (Nothing, Nothing)
    tickleIfExists (Just (_, v)) = (Just v,  Just (k, nextTick, v))

lookupNoLRU :: Int -> LRUCache v -> Maybe v
lookupNoLRU k = IntPSQ.lookup k . lrucPsq

increaseTick :: Tick -> IntPSQ Tick v -> Size -> LRUCache v
increaseTick tick psq maxSize
  | tick < maxBound = LRUCache (tick + 1) psq maxSize
  | otherwise       = retick old IntPSQ.empty 0
  where
    retick !oldPsq !newPsq !newTick = case IntPSQ.minView oldPsq of
      Nothing                 -> LRUCache newTick newPsq maxSize
      Just (k, _, v, oldPsq') ->
        retick oldPsq' (IntPSQ.insert k newTick v newPsq) (newTick + 1)

insert :: Int -> v -> LRUCache v -> (LRUCache v, Maybe (k, v))
insert k v (LRUCache nextTick psq maxSize) =
    | IntPSQ.size psq' <= maxSize =
        (increaseTick nextTick psq' maxSize, Nothing)
    | otherwise                  =
        fromMaybe (LRUCache 0 IntPSQ.empty maxSize, Nothing) $ do
            ((k, _, v), psq'') <- IntPSQ.minViewWithKey psq'
            return (increaseTick nextTick psq'' maxSize, Just (k, v))
   where
     psq' = IntPSQ.insert k nextTick v psq

-- | Evict the entry at the given key in the cache.
evict :: Int -> LRUCache v -> (LRUCache v, Maybe v)
evict k cache = case IntPSQ.deleteView k (lrucPsq cache) of
    (_,    Nothing       -> (cache,                  Nothing)
    (psq', Just (_t, v)) -> (cache {lrucPsq = psq'}, Just v)

evictOldest :: LRUCache v -> (LRUCache v, Maybe (k, v))
evictOldest (LRUCache nextTick psq maxSize) =
    fromMaybe (LRUCache 0 IntPSQ.empty maxSize, Nothing) $ do
        ((k, _, v), psq') <- IntPSQ.minViewWithKey psq
        return (LRUCache nextTick psq' maxSize, Just (k, v))
