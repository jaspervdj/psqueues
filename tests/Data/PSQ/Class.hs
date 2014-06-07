-- | Generic class with properties and methods that are available for all
-- different implementations ('IntPSQ', 'PSQ' and 'HashPSQ').
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
import qualified Data.PSQ      as PSQ

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
    deleteView
        :: Ord p => Key psq -> psq p v -> Maybe (p, v, psq p v)
    minView
        :: Ord p => psq p v -> Maybe (Key psq, p, v, psq p v)

    -- Traversals
    map :: Ord p => (Key psq -> p -> v -> w) -> psq p v -> psq p w
    fold'
        :: Ord p => (Key psq -> p -> v -> a -> a) -> a -> psq p v -> a

instance PSQ IntPSQ.IntPSQ where
    type Key IntPSQ.IntPSQ = Int

    null       = IntPSQ.null
    size       = IntPSQ.size
    member     = IntPSQ.member
    lookup     = IntPSQ.lookup
    findMin    = IntPSQ.findMin
    empty      = IntPSQ.empty
    singleton  = IntPSQ.singleton
    insert     = IntPSQ.insert
    delete     = IntPSQ.delete
    alter      = IntPSQ.alter
    alterMin   = IntPSQ.alterMin
    fromList   = IntPSQ.fromList
    toList     = IntPSQ.toList
    keys       = IntPSQ.keys
    deleteView = IntPSQ.deleteView
    minView    = IntPSQ.minView
    map        = IntPSQ.map
    fold'      = IntPSQ.fold'

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

instance forall k. (Hashable k, Ord k) => PSQ (HashPSQ.HashPSQ k) where
    type Key (HashPSQ.HashPSQ k) = k

    empty       = HashPSQ.empty
    singleton   = HashPSQ.singleton
    fromList    = HashPSQ.fromList

    lookup = HashPSQ.lookup
