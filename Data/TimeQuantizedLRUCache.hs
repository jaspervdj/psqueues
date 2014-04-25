
module Data.TimeQuantizedLRUCache
  ( Config(..)

  , Handle
  , new
  , insertJoined
  , lookup
  , evict
  ) where


import           Prelude.Extended              hiding (lookup)

import qualified Data.Configurator.Extended    as C
import           Data.IORef                    (IORef, newIORef, readIORef, atomicModifyIORef')
import qualified Data.Text                     as T
import qualified Data.PSQueue                  as PSQ
import           System.Random                 (StdGen, mkStdGen, randomR)

import           Erudify.Pure.UnixTime         (UnixTime)
import qualified Erudify.Pure.UnixTime         as UnixTime
import qualified Erudify.System.Clock          as Clock
import qualified Erudify.System.Ekg            as Ekg
import qualified Erudify.System.PseudoRandom   as Random


-- Configuration
----------------

data Config = Config
    { cMaxNumEntries            :: {-# UNPACK #-} !Int
      -- ^ The maximal number of distinct entries that the cache must be able
      -- to store.
    , cMaxSize                  :: {-# UNPACK #-} !Int
      -- ^ The maximal size of the cache in bytes.
    , cMaxLastAccessImprecision :: {-# UNPACK #-} !Int
      -- ^ Maximal time in milliseconds that can lie between the current and
      -- the last access to an entry.
    }

{-
parseConfig :: C.Config -> IO Config
parseConfig cfg =
    Config
      <$> C.require cfg "maxNumEntries"
      <*> C.require cfg "maxAge"
-}


-- Types
--------

-- | A cache entry with an expiry-time measured in milliseconds since the
-- cache creation.
data Entry v = Entry
    { _eLastAccessedAt :: {-# UNPACK #-} !Int
    , _eValue          :: !v
    }
    deriving (Eq, Ord)
    -- NOTE that we rely on 'deriving Ord' to use the lexicographic order of
    -- the constructor arguments in the order they are specified.

{- TODO (SM): continue once we have the HashPSQ

type Cache k v = PSQ.PSQ k (Entry v)

-- | A type with explicit strictness and unpacking annotations to speed up
-- passing the result through the 'atomicModifyIORef'.
data PureInsertResult k v = PureInsertResult
    { _pirValue      ::                !v
    , pirCache       ::                !(Cache k v)
    , _pirNumExpired :: {-# UNPACK #-} !Int
    , _pirNumEvicted :: {-# UNPACK #-} !Int
    }

data Metrics = Metrics
    { metricsNumExpired :: !Ekg.CounterRef
      -- ^ Incremented when an entry is removed because it expired.
    , metricsNumEvicted   :: !Ekg.CounterRef
      -- ^ Incremented when an entry is removed because there was not enough
      -- space left.

    , metricsNumEntries     :: !Ekg.GaugeRef
    , metricsLookupHits     :: !Ekg.CounterRef
    , metricsLookupMisses   :: !Ekg.CounterRef
    , metricsInserts        :: !Ekg.CounterRef
    , metricsFlushes        :: !Ekg.CounterRef

    , metricsNumThreadsModifyingCache :: !Ekg.GaugeRef
    }


-- Creation
-----------

data Handle k v = Handle
    { hMaxSize   :: {-# UNPACK #-} !Int
    , hMaxAge    ::                !Int
    , hCreatedAt ::                !UnixTime
      -- ^ When the cache was created. We use this as a new zero time to allow
      -- using 'Int's to measure the age of entries.
    , hCache     :: {-# UNPACK #-} !(IORef (Cache k v))
    , hClock     ::                !Clock.Handle
    , hRandom    ::                !(IORef StdGen)
    , hMetrics   ::                !Metrics
    }

-- | Create a new handle to a named expiring cache.
new :: (Ord k, Ord v)
    => Config -> T.Text -> Clock.Handle -> Random.Handle -> Ekg.Handle -> IO (Handle k v)
new config@(Config maxSize maxAge) name clock prng ekg = do
    -- NOTE (SM) PSQ.empty strangely requires Ord constraints :-/
    cacheRef  <- newIORef PSQ.empty
    metrics   <- newMetrics config name ekg
    createdAt <- Clock.getCurrentUnixTime clock
    stdGen    <- newIORef =<< (mkStdGen <$> Random.uniform prng)
    return (Handle maxSize maxAge createdAt cacheRef clock stdGen metrics)

newMetrics :: Config -> T.Text -> Ekg.Handle -> IO Metrics
newMetrics config prefix ekg = do
    maxNumEntries       <- getGauge "maxNumEntries"
    maxEntryAge         <- getGauge "maxEntryAge_ms"
    Ekg.unsafeSetGauge maxNumEntries (cMaxNumEntries config)
    Ekg.unsafeSetGauge maxEntryAge   (cMaxAge config)

    numExpired               <- getCounter "expiredEntries"
    numEvicted               <- getCounter "evictedEntries"
    cacheSize                <- getGauge   "numEntries"
    lookupHits               <- getCounter "lookup_hits"
    lookupMisses             <- getCounter "lookup_misses"
    inserts                  <- getCounter "inserts"
    flushes                  <- getCounter "flushes"
    numThreadsModifyingCache <- getGauge   "numThreadsModifyingCache"
    return $ Metrics
        { metricsNumExpired               = numExpired
        , metricsNumEvicted               = numEvicted
        , metricsNumEntries               = cacheSize
        , metricsLookupHits               = lookupHits
        , metricsLookupMisses             = lookupMisses
        , metricsInserts                  = inserts
        , metricsFlushes                  = flushes
        , metricsNumThreadsModifyingCache = numThreadsModifyingCache
        }
  where
    getCounter name = Ekg.getCounterRef ekg (prefix <> "_" <> name)
    getGauge name = Ekg.getGaugeRef ekg (prefix <> "_" <> name)



-- Main interface
-----------------

-- | Internal function to share the computation of the cache age.
getCacheAge :: Handle k v -> IO Int
getCacheAge handle = do
    now <- Clock.getCurrentUnixTimeCached $ hClock handle
    return (fromIntegral (now `UnixTime.diff` hCreatedAt handle))

-- | Return the value associated to the given key in the cache, provided the
-- cache entry is not expired.
lookup :: (Ord k, Ord v) => Handle k v -> k -> IO (Maybe v)
lookup handle k = do
    -- Note that we never expire the cache here, as we want positive lookups
    -- (the most common case) to be as fast as possible. If an entry is
    -- expired, then there's anyways a following insert coming, which will
    -- remove this and all other expired entries.
    now   <- getCacheAge handle
    cache <- readIORef (hCache handle)
    case pureLookup now cache of
      Nothing      -> do Ekg.incCounter (metricsLookupMisses metrics)
                         return Nothing
      mbV@(Just _) -> do Ekg.incCounter (metricsLookupHits metrics)
                         return mbV
  where
    metrics = hMetrics handle

    pureLookup !now !cache =
        case PSQ.lookup k cache of
          Just (ExpiringEntry expiresAt v) | now < expiresAt -> Just v
          _                                                  -> Nothing


-- | Evict all cache entries.
flush :: (Ord k, Ord v) => Handle k v -> IO ()
flush handle = do
    -- NOTE (SM) PSQ.empty strangely requires Ord constraints :-/
    numEvicted <- monitoredModifyCache handle $ \cache -> (PSQ.empty, PSQ.size cache)
    Ekg.unsafeSetGauge (metricsNumEntries metrics) 0
    Ekg.incCounter (metricsFlushes metrics)
    Ekg.addCounter (metricsNumEvicted metrics) numEvicted
  where
    metrics = hMetrics handle


-- Insert
---------

-- | Insert a value at a specific key and joining the value with the possibly
-- existing value in the cache using the given idempotent and commutative
-- function.
--
-- Note that using an idempotent and commutative function means that we can
-- perform concurrent updates in a race-free fashion.
insertJoined :: (Ord k, Ord v) => Handle k v -> (v -> v -> v) -> k -> v -> IO v
insertJoined handle joinValues k v = do
    Ekg.incCounter $ metricsInserts (hMetrics handle)
    now <- getCacheAge handle

    -- We reduce TTL by a random amount (max. 10%) so that groups of entries
    -- don't all expire at the same time.
    fuzz <- atomicModifyIORef' (hRandom handle) $ \stdgen ->
              let (!val, !stdgen') = randomR (0, hMaxAge handle `div` 10) stdgen in
              (stdgen', val)

    PureInsertResult insertedV cache numExpired numEvicted <-
        monitoredModifyCache handle $ \cache ->
           let result = pureInsertJoined (hMaxSize handle) joinValues now
                                         k v (now + hMaxAge handle - fuzz) cache
           in  (pirCache result, result)

    Ekg.unsafeSetGauge (metricsNumEntries metrics) (PSQ.size cache)
    Ekg.addCounter (metricsNumExpired metrics) numExpired
    Ekg.addCounter (metricsNumEvicted metrics) numEvicted

    return insertedV
  where
    metrics = hMetrics handle

-- | Expire all entries that are too old and return both the number of expired
-- entries and the new cache.
removeExpiredEntries :: (Ord k, Ord v) => Int -> Cache k v -> (Cache k v, Int)
removeExpiredEntries now =
    go 0
  where
    go !numExpired cache = case PSQ.minView cache of
      Just (_ PSQ.:-> (ExpiringEntry expiresAt _), cache')
        | expiresAt <= now -> go (numExpired + 1) cache'
        | otherwise        -> (cache, numExpired)
      Nothing              -> (cache, numExpired)


-- | Insert a value and join it with a possibly existing previous value.
pureInsertJoined
    :: (Ord k, Ord v)
    => Int           -- ^ Maximal size
    -> (v -> v -> v) -- ^ How to joing the values
    -> Int           -- ^ Current cache age
    -> k -> v -> Int -- Key, value, expiry age
    -> Cache k v -> PureInsertResult k v
pureInsertJoined !maxSize joinValues !now !k !v !expiresAt !cache0 =
    case PSQ.lookup k cache of
      Just (ExpiringEntry oldExpiresAt oldV) ->
          let !joinedV = joinValues oldV v
              -- NOTE that we must combine the expiry times in order to
              -- guarantee a race-free update.
              !entry   = ExpiringEntry (max oldExpiresAt expiresAt) joinedV
          in  PureInsertResult joinedV (PSQ.insert k entry cache) numExpired 0
      Nothing
        | PSQ.size cache < maxSize ->
            PureInsertResult v (insertNewEntry cache)                 numExpired 0
        | otherwise                ->
            PureInsertResult v (insertNewEntry (PSQ.deleteMin cache)) numExpired 1
  where
    (cache, numExpired) = removeExpiredEntries now cache0
    insertNewEntry      = PSQ.insert k (ExpiringEntry expiresAt v)

-- Metrics utilities
--------------------

monitoredModifyCache :: Handle k v -> (Cache k v -> (Cache k v, a)) -> IO a
monitoredModifyCache handle f =
    -- We use atomicModifyIORef to get the precise number of evicted entries.
    Ekg.monitorSection (metricsNumThreadsModifyingCache (hMetrics handle)) $
        atomicModifyIORef' (hCache handle) f


-}
