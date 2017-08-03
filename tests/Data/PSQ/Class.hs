-- | Generic class with properties and methods that are available for all
-- different implementations ('IntPSQ', 'OrdPSQ' and 'HashPSQ').
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.PSQ.Class
    ( PSQ (..)
    ) where

import           Data.Hashable (Hashable)

import qualified Data.IntPSQ   as IntPSQ
import qualified Data.HashPSQ  as HashPSQ
import qualified Data.OrdPSQ   as OrdPSQ

class PSQ (psq :: * -> * -> *) where
    type Key psq :: *

    -- Query
    null
        :: Ord p => psq p v -> Bool
    size
        :: Ord p => psq p v -> Int
    member
        :: Ord p => Key psq -> psq p v -> Bool
    lookup
        :: Ord p => Key psq -> psq p v -> Maybe (p, v)
    findMin
        :: Ord p => psq p v -> Maybe (Key psq, p, v)

    -- Construction
    empty
        :: Ord p => psq p v
    singleton
        :: Ord p => Key psq -> p -> v -> psq p v

    -- Insertion
    insert
        :: Ord p => Key psq -> p -> v -> psq p v -> psq p v

    -- Delete/update
    delete
        :: Ord p => Key psq -> psq p v -> psq p v
    deleteMin
        :: Ord p => psq p v -> psq p v
    alter
        :: Ord p
        => (Maybe (p, v) -> (b, Maybe (p, v)))
        -> Key psq -> psq p v -> (b, psq p v)
    alterMin
        :: Ord p
        => (Maybe (Key psq, p, v) -> (b, Maybe (Key psq, p, v)))
        -> psq p v -> (b, psq p v)

    -- Lists
    fromList
        :: Ord p => [(Key psq, p, v)] -> psq p v
    toList
        :: Ord p => psq p v -> [(Key psq, p, v)]
    keys
        :: Ord p => psq p v -> [Key psq]

    -- Views
    insertView
        :: Ord p => Key psq -> p -> v -> psq p v -> (Maybe (p, v), psq p v)
    deleteView
        :: Ord p => Key psq -> psq p v -> Maybe (p, v, psq p v)
    minView
        :: Ord p => psq p v -> Maybe (Key psq, p, v, psq p v)
    atMostView
        :: Ord p => p -> psq p v -> ([(Key psq, p, v)], psq p v)

    -- Traversals
    map :: Ord p => (Key psq -> p -> v -> w) -> psq p v -> psq p w
    unsafeMapMonotonic
        :: (Ord p, Ord q) => (Key psq -> p -> v -> (q, w)) -> psq p v -> psq q w
    fold'
        :: Ord p => (Key psq -> p -> v -> a -> a) -> a -> psq p v -> a

    -- Validity check
    valid
        :: Ord p => psq p v -> Bool

instance PSQ IntPSQ.IntPSQ where
    type Key IntPSQ.IntPSQ = Int

    null               = IntPSQ.null
    size               = IntPSQ.size
    member             = IntPSQ.member
    lookup             = IntPSQ.lookup
    findMin            = IntPSQ.findMin
    empty              = IntPSQ.empty
    singleton          = IntPSQ.singleton
    insert             = IntPSQ.insert
    delete             = IntPSQ.delete
    deleteMin          = IntPSQ.deleteMin
    alter              = IntPSQ.alter
    alterMin           = IntPSQ.alterMin
    fromList           = IntPSQ.fromList
    toList             = IntPSQ.toList
    keys               = IntPSQ.keys
    insertView         = IntPSQ.insertView
    deleteView         = IntPSQ.deleteView
    minView            = IntPSQ.minView
    atMostView         = IntPSQ.atMostView
    map                = IntPSQ.map
    unsafeMapMonotonic = IntPSQ.unsafeMapMonotonic
    fold'              = IntPSQ.fold'
    valid              = IntPSQ.valid

instance forall k. Ord k => PSQ (OrdPSQ.OrdPSQ k) where
    type Key (OrdPSQ.OrdPSQ k) = k

    null               = OrdPSQ.null
    size               = OrdPSQ.size
    member             = OrdPSQ.member
    lookup             = OrdPSQ.lookup
    findMin            = OrdPSQ.findMin
    empty              = OrdPSQ.empty
    singleton          = OrdPSQ.singleton
    insert             = OrdPSQ.insert
    delete             = OrdPSQ.delete
    deleteMin          = OrdPSQ.deleteMin
    alter              = OrdPSQ.alter
    alterMin           = OrdPSQ.alterMin
    fromList           = OrdPSQ.fromList
    toList             = OrdPSQ.toList
    keys               = OrdPSQ.keys
    insertView         = OrdPSQ.insertView
    deleteView         = OrdPSQ.deleteView
    minView            = OrdPSQ.minView
    atMostView         = OrdPSQ.atMostView
    map                = OrdPSQ.map
    unsafeMapMonotonic = OrdPSQ.unsafeMapMonotonic
    fold'              = OrdPSQ.fold'
    valid              = OrdPSQ.valid

instance forall k. (Hashable k, Ord k) => PSQ (HashPSQ.HashPSQ k) where
    type Key (HashPSQ.HashPSQ k) = k

    null               = HashPSQ.null
    size               = HashPSQ.size
    member             = HashPSQ.member
    lookup             = HashPSQ.lookup
    findMin            = HashPSQ.findMin
    empty              = HashPSQ.empty
    singleton          = HashPSQ.singleton
    insert             = HashPSQ.insert
    delete             = HashPSQ.delete
    deleteMin          = HashPSQ.deleteMin
    alter              = HashPSQ.alter
    alterMin           = HashPSQ.alterMin
    fromList           = HashPSQ.fromList
    toList             = HashPSQ.toList
    keys               = HashPSQ.keys
    insertView         = HashPSQ.insertView
    deleteView         = HashPSQ.deleteView
    minView            = HashPSQ.minView
    atMostView         = HashPSQ.atMostView
    map                = HashPSQ.map
    unsafeMapMonotonic = HashPSQ.unsafeMapMonotonic
    fold'              = HashPSQ.fold'
    valid              = HashPSQ.valid
