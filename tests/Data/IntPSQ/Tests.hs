{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.IntPSQ.Tests
    ( tests
    ) where

import           Test.QuickCheck                      (Property, forAll)
import           Test.Framework                       (Test)
import           Test.Framework.Providers.QuickCheck2 (testProperty)

import           Data.IntPSQ.Internal
import           Data.PSQ.Class.Gen

--------------------------------------------------------------------------------
-- Index of tests
--------------------------------------------------------------------------------

tests :: [Test]
tests =
    [ testProperty "prop_valid" prop_valid
    ]

--------------------------------------------------------------------------------
-- QuickCheck properties
--------------------------------------------------------------------------------

prop_valid :: Property
prop_valid = forAll arbitraryPSQ $ \t ->
    valid (t :: IntPSQ Int Char)
