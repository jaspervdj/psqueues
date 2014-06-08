module Data.IntPSQ.Tests
    ( tests
    ) where

import           Prelude hiding (lookup)

import           Test.QuickCheck                      (Property, arbitrary,
                                                       forAll)
import           Test.Framework                       (Test)
import           Test.Framework.Providers.QuickCheck2 (testProperty)

import           Data.IntPSQ.Internal
import           Data.PSQ.Class.Gen

--------------------------------------------------------------------------------
-- Index of tests
--------------------------------------------------------------------------------

tests :: [Test]
tests =
    [ testProperty "valid"                   prop_valid
    , testProperty "insertLargerThanMaxPrio" prop_insertLargerThanMaxPrio
    ]

--------------------------------------------------------------------------------
-- QuickCheck properties
--------------------------------------------------------------------------------

prop_valid :: Property
prop_valid = forAll arbitraryPSQ $ \t ->
    valid (t :: IntPSQ Int Char)

prop_insertLargerThanMaxPrio :: Property
prop_insertLargerThanMaxPrio =
    forAll arbitraryPSQ $ \t ->
    forAll arbitrary    $ \k ->
    forAll arbitrary    $ \x ->
        let max' x Nothing  = Just x
            max' x (Just y) = Just (max x y)
            maxPriority     = fold' (\x _ _ acc -> max' x acc) Nothing t
            priority        = maybe 3 (+ 1) maxPriority
            t'              = insertLargerThanMaxPrio k priority x t
        in valid (t' :: IntPSQ Int Char) && lookup k t' == Just (priority, x)
