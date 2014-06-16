{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.IntPSQ
    ( -- * Type
      IntPSQ

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
    ) where

import           Prelude hiding (lookup, map, filter, foldr, foldl, null)

import           Data.IntPSQ.Internal
