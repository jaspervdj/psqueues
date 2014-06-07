-- | Generic class with properties and methods that are available for all
-- different implementations ('IntPSQ', 'PSQ' and 'HashPSQ').
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.PSQ.Class
    ( PSQ (..)
    ) where

import qualified Data.IntPSQ as IntPSQ
import qualified Data.PSQ    as PSQ

class PSQ (psq :: * -> * -> *) where
    type Key psq :: *

    empty     :: Ord p => psq p v
    singleton :: Ord p => Key psq -> p -> v -> psq p v
    fromList  :: Ord p => [(Key psq, p, v)] -> psq p v

    null   :: Ord p => psq p v -> Bool
    size   :: Ord p => psq p v -> Int
    toList :: Ord p => psq p v -> [(Key psq, p, v)]
    lookup :: Ord p => Key psq -> psq p v -> Maybe (p, v)

    insert :: Ord p => Key psq -> p -> v -> psq p v -> psq p v
    delete :: Ord p => Key psq -> psq p v -> psq p v

    minViewWithKey :: Ord p => psq p v -> Maybe ((Key psq, p, v), psq p v)

instance PSQ IntPSQ.IntPSQ where
    type Key IntPSQ.IntPSQ = Int

    empty     = IntPSQ.empty
    singleton = IntPSQ.singleton
    fromList  = IntPSQ.fromList

    null   = IntPSQ.null
    size   = IntPSQ.size
    toList = IntPSQ.toList
    lookup = IntPSQ.lookup

    insert = IntPSQ.insert
    delete = IntPSQ.delete

    minViewWithKey = IntPSQ.minViewWithKey

instance forall k. Ord k => PSQ (PSQ.PSQ k) where
    type Key (PSQ.PSQ k) = k

    empty     = PSQ.empty
    singleton = PSQ.singleton
    fromList  = PSQ.fromList

    null   = PSQ.null
    size   = PSQ.size
    toList = PSQ.toList
    lookup = PSQ.lookup

    -- TODO: Add these functions once they have the proper API
    -- insert  = PSQ.insert
    -- delete  = PSQ.delete

    -- minViewWithKey = PSQ.minViewWithKey


