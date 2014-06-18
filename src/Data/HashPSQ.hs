module Data.HashPSQ
    ( -- * Type
      HashPSQ

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

      -- * Delete/update
    , delete
    , alter
    , alterMin

      -- * Lists
    , fromList
    , toList
    , keys

      -- * Views
    , insertView
    , deleteView
    , minView

      -- * Traversal
    , map
    , fold'

      -- * Unsafe operations
    , unsafeLookupIncreasePriority
    , unsafeInsertIncreasePriority
    , unsafeInsertIncreasePriorityView

      -- * Validity check
    , valid
    ) where

import           Prelude               hiding (foldr, lookup, map, null)

import           Data.HashPSQ.Internal
