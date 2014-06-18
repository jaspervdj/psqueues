{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy         #-}
{-# LANGUAGE BangPatterns        #-}
module Data.OrdPSQ
    ( -- * Type
      OrdPSQ

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

      -- * Delete/Update
    , delete
    , alter
    , alterMin

      -- * Conversion
    , fromList
    , toList
    , toAscList
    , keys

      -- * Views
    , insertView
    , deleteView
    , minView

      -- * Traversals
    , map
    , fold'

      -- * Validity check
    , valid
    ) where

import           Prelude hiding (map, lookup, null, foldr)

import           Data.OrdPSQ.Internal
