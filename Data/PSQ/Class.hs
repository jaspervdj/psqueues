-- | Generic class with properties and methods that are available for all
-- different implementations ('IntPSQ', 'PSQ' and 'HashPSQ').
{-# LANGUAGE TypeFamilies #-}
module Data.PSQ.Class
    ( PSQ (..)
    ) where

import qualified Data.IntPSQ as IntPSQ

class PSQ (psq :: * -> * -> *) where
    type Key :: *

    empty     :: Ord p => psq p v
    singleton :: Ord p => Key -> p -> v -> psq p v
    fromList  :: Ord p => [(Key, p, v)] -> psq p v

    null   :: Ord p => psq p v -> Bool
    size   :: Ord p => psq p v -> Int
    toList :: Ord p => psq p v -> [(Key, p, v)]

    insert :: Ord p => Key -> p -> v -> psq p v -> psq p v
    delete :: Ord p => Key -> psq p v -> psq p v

    minViewWithKey :: Ord p => psq p v -> Maybe ((Key, p, v), psq p v)

instance PSQ IntPSQ.IntPSQ where
    type Key = Int

    empty     = IntPSQ.empty
    singleton = IntPSQ.singleton
    fromList  = IntPSQ.fromList

    null   = IntPSQ.null
    size   = IntPSQ.size
    toList = IntPSQ.toList

    insert = IntPSQ.insert
    delete = IntPSQ.delete

    minViewWithKey = IntPSQ.minViewWithKey
