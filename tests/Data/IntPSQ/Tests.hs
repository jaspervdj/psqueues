module Data.IntPSQ.Tests
    ( tests
    ) where

import           Prelude hiding (lookup)

import           Test.QuickCheck                      (Property, arbitrary,
                                                       forAll)
import           Test.Framework                       (Test)
import           Test.Framework.Providers.HUnit       (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.HUnit                           (Assertion, assert)

import           Data.IntPSQ.Internal
import           Data.PSQ.Class.Gen

--------------------------------------------------------------------------------
-- Index of tests
--------------------------------------------------------------------------------

tests :: [Test]
tests =
    [ testCase     "hasBadNils"                 test_hasBadNils
    , testProperty "valid"                      prop_valid
    , testProperty "insertLargerThanMaxPrio"    prop_insertLargerThanMaxPrio
    , testProperty "insertLargerThanMaxPrioView"
                                                prop_insertLargerThanMaxPrioView
    ]


--------------------------------------------------------------------------------
-- Util
--------------------------------------------------------------------------------

largerThanMaxPrio :: IntPSQ Int Char -> Int
largerThanMaxPrio = maybe 3 (+ 1) . fold' (\_ p _ acc -> max' p acc) Nothing
  where
    max' x Nothing  = Just x
    max' x (Just y) = Just (max x y)


--------------------------------------------------------------------------------
-- Unit tests
--------------------------------------------------------------------------------

-- 100% test coverage...
test_hasBadNils :: Assertion
test_hasBadNils =
    assert $ hasBadNils (Bin 1 (2 :: Int) 'x' 0 Nil Nil)


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
        let priority = largerThanMaxPrio t
            t'       = unsafeInsertLargerThanMaxPrio k priority x t
        in valid (t' :: IntPSQ Int Char) && lookup k t' == Just (priority, x)

prop_insertLargerThanMaxPrioView :: Property
prop_insertLargerThanMaxPrioView =
    forAll arbitraryPSQ $ \t ->
    forAll arbitrary    $ \k ->
    forAll arbitrary    $ \x ->
        let priority   = largerThanMaxPrio t
            (mbPx, t') = unsafeInsertLargerThanMaxPrioView k priority x t
        in valid (t' :: IntPSQ Int Char) &&
            lookup k t' == Just (priority, x) &&
            lookup k t  == mbPx
