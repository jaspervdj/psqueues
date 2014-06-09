{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy         #-}
{-# LANGUAGE BangPatterns        #-}
module Data.PSQ
    ( -- * Type
      PSQ

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
    , deleteView
    , minView

      -- * Traversals
    , map
    , fold'
    ) where

import           Prelude hiding (map, lookup, null, foldr)

import           Data.PSQ.Internal
