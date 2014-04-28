{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}

-- | Cache contention benchmark that compares different alternatives for
-- managing access to the shared cache.
module Main where

import           Control.Applicative
import           Control.DeepSeq (rnf)
import           Control.Monad
import           Control.Concurrent.Async (mapConcurrently)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import           Control.Exception (evaluate)

import           Data.Atomics       (atomicModifyIORefCAS_)
import qualified Data.IntMap.Strict as IMS
import           Data.IORef
import qualified Data.Vector        as V

import           Data.Time
import           System.Environment
import           System.Random.Shuffle (shuffleM)

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

-- | Fixed version of 'shuffleM'
shuffleM' :: [a] -> IO [a]
shuffleM' [] = return []
shuffleM' xs = shuffleM xs

-- | Make the duration of an IO operation observable on stdout.
timed :: String -> IO a -> IO a
timed name io = do
    putStr $ "Running '" ++ name ++ "'... "
    t0 <- getCurrentTime
    x  <- io
    t1 <- getCurrentTime
    putStrLn $ " done in " ++ show (t1 `diffUTCTime` t0)
    return x



------------------------------------------------------------------------------
-- Reference types
------------------------------------------------------------------------------

-- | An abstract description of a reference to a mutable variable.
data Ref a = forall r. Ref String (a -> IO r) (r -> IO a) (r -> (a -> a) -> IO ())

ioRef :: Ref a
ioRef = Ref "IORef" newIORef readIORef atomicModifyIORef'_

ioRefCas :: Ref a
ioRefCas = Ref "IORef CAS" newIORef readIORef atomicModifyIORefCAS_

tVar :: Ref a
tVar = Ref "TVar" newTVarIO readTVarIO atomicModifyTVar'_

tVarLazy :: Ref a
tVarLazy = Ref "TVar Lazy" newTVarIO readTVarIO atomicModifyTVar_

mVar :: Ref a
mVar = Ref "MVar" newMVar readMVar atomicModifyMVar'_

atomicModifyIORef'_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef'_ ref f = atomicModifyIORef' ref (\x -> (f x, ()))


atomicModifyTVar'_ :: TVar a -> (a -> a) -> IO ()
atomicModifyTVar'_ ref f = atomically $ do
    x <- readTVar ref
    x' <- return =<< (return $! f x)
    writeTVar ref x'

atomicModifyTVar_ :: TVar a -> (a -> a) -> IO ()
atomicModifyTVar_ ref f = do
    x <- atomically $ do
        x <- readTVar ref
        writeTVar ref (f x)
        return x
    void $ evaluate x

atomicModifyMVar'_ :: MVar a -> (a -> a) -> IO ()
atomicModifyMVar'_ ref f = modifyMVar_ ref $ \x -> evaluate (f x)


------------------------------------------------------------------------------
-- Cache access simulation
------------------------------------------------------------------------------

-- | Generate a list of operations that a thread should execute. 'Left's are
-- used to denote lookups, which are constructed to be successful. 'Right's
-- denote remove element, insert new element pairs. Together these operations
-- simulate the typical use of a centralized cache.
genThreadOperations :: Int -> Int -> Int -> Int -> IO [Either Int (Int, Int)]
genThreadOperations numThreads numLookups numUpdates threadId = do
    shuffleM' $ map Right (zip updateIdxs (tail updateIdxs)) ++
                map Left  (mkLookupIdx numThreads <$> [1..numLookupsPerThread])
  where
    updateIdxs = [ i * (numThreads + 1) + threadId + 1
               | i <- [0..numUpdatesPerThread]
               ]

    numUpdatesPerThread = numUpdates `div` numThreads
    numLookupsPerThread = numLookups `div` numThreads

mkLookupIdx :: Int -> Int -> Int
mkLookupIdx numThreads i = i * (numThreads + 1)

-- | The elemnts that are initially inserted into the state.
genInitialElems :: Int -> Int -> IO [Int]
genInitialElems numThreads numInitialElems =
    shuffleM' $ mkLookupIdx numThreads <$> [1..numInitialElems]

-- | Simulate concurrent cache access.
{-# INLINE simulateCacheOperations #-}
simulateCacheOperations
    :: String
    -> IO b
    -> (b -> Int -> IO ())
    -> (b -> Int -> IO ())
    -> (b -> Int -> IO ())
    -> Int                           -- ^ Number of stripes
    -> [Int]                         -- ^ Intial elements to insert
    -> [[Either Int (Int, Int)]]     -- ^ Operations to execute for each thread
    -> IO ()
simulateCacheOperations
    name newState
    lookup0 insert0 delete0
    numStripes
    initialElems operationss
  = do
    putStr "- "
    stripes <- V.fromList <$> replicateM numStripes newState

    let stripe i = stripes V.! (i `mod` numStripes)
        lookup i = lookup0 (stripe i) i
        insert i = insert0 (stripe i) i
        delete i = delete0 (stripe i) i

    -- populate initial state
    mapM_ insert initialElems -- (initialElems numThreads numInitialElems)

    -- create jobs
    jobs <- forM operationss $ \operations -> do
        -- ops <- threadOperations numThreads numLookups numUpdates threadIdx
        return $ do
            forM_ operations $ \op -> case op of
              Left i        -> lookup i
              Right (i, i') -> do
                -- We undo the previous inserts to keep the state sizes
                -- constant. This simplifies comparisons between runs with
                -- different numbers of updates
                delete i
                insert i'

    -- run jobs
    void $ timed name $ mapConcurrently id jobs


main :: IO ()
main = do
    args <- getArgs
    case args of
      [  read -> numThreads
       , read -> numStripes
       , read -> numInitialElems
       , read -> numLookups
       , read -> numUpdates
       ] -> do
              putStrLn ""
              putStrLn $ unlines
                [ "Number of threads:       " ++ show numThreads
                , "Number of stripes:       " ++ show numStripes
                , "Number of initial elems: " ++ show numInitialElems
                , "Number of lookups:       " ++ show numLookups
                , "Number of updates:       " ++ show numUpdates
                ]

              initialElems <- genInitialElems numThreads numInitialElems
              opss         <- forM [0..numThreads - 1] $ \threadIdx ->
                  genThreadOperations numThreads numLookups numUpdates threadIdx

              evaluate (rnf (initialElems, opss))

              forM_ [ioRefCas, tVar, ioRef, tVarLazy, mVar] $
                  \(Ref refName newRef readRef modifyRef) ->

                      simulateCacheOperations
                          ( refName ++ " / IntMap")
                          (newRef IMS.empty)
                          (\ref i -> do im <- readRef ref
                                        void $ evaluate (IMS.lookup i im)
                          )
                          (\ref i -> modifyRef ref (IMS.insert i i))
                          (\ref i -> modifyRef ref (IMS.delete i))
                          numStripes
                          initialElems
                          opss
              putStrLn ""


      _ -> do putStrLn $ "ERROR: could not parse arguments " ++ show args
              putStrLn $ "Expecting five ints:"
              putStrLn $ "  numThreads, numStripes, numInitialElems, numLookups, numUpdates"
              putStrLn ""

