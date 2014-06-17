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
    [ testCase     "hasBadNils"     test_hasBadNils
    , testProperty "valid"          prop_valid
    , testProperty "unsafeInsertLargerThanMaxPrio"
                                    prop_unsafeInsertLargerThanMaxPrio
    , testProperty "unsafeInsertLargerThanMaxPrioView"
                                    prop_unsafeInsertLargerThanMaxPrioView
    , testProperty "unsafeInsertWithLargerThanMaxPrio"
                                    prop_unsafeInsertWithLargerThanMaxPrio
    , testProperty "unsafeInsertWithLargerThanMaxPrioView"
                                    prop_unsafeInsertWithLargerThanMaxPrioView
    ]


--------------------------------------------------------------------------------
-- Util
--------------------------------------------------------------------------------

largerThanMaxPrio :: IntPSQ Int v -> Int
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

prop_unsafeInsertLargerThanMaxPrio :: Property
prop_unsafeInsertLargerThanMaxPrio =
    forAll arbitraryPSQ $ \t ->
    forAll arbitrary    $ \k ->
    forAll arbitrary    $ \x ->
        let prio = largerThanMaxPrio t
            t'   = unsafeInsertLargerThanMaxPrio k prio x t
        in valid (t' :: IntPSQ Int Char) && lookup k t' == Just (prio, x)

prop_unsafeInsertLargerThanMaxPrioView :: Property
prop_unsafeInsertLargerThanMaxPrioView =
    forAll arbitraryPSQ $ \t ->
    forAll arbitrary    $ \k ->
    forAll arbitrary    $ \x ->
        let prio       = largerThanMaxPrio t
            (mbPx, t') = unsafeInsertLargerThanMaxPrioView k prio x t
        in valid (t' :: IntPSQ Int Char) &&
            lookup k t' == Just (prio, x) &&
            lookup k t  == mbPx

prop_unsafeInsertWithLargerThanMaxPrio :: Property
prop_unsafeInsertWithLargerThanMaxPrio =
    forAll arbitraryPSQ $ \t0 ->
    forAll arbitrary    $ \k  ->
    forAll arbitrary    $ \x  ->
        let t      = fmap (\e -> [e]) t0 :: IntPSQ Int [Char]
            prio   = largerThanMaxPrio t
            f      = \newP newX oldP oldX ->
                        (newP + (abs oldP `div` 2), newX ++ oldX)
            t'     = unsafeInsertWithLargerThanMaxPrio f k prio [x] t
            expect = case lookup k t of
                            Nothing     -> (prio, [x])
                            Just (p, y) -> (prio + (abs p `div` 2), [x] ++ y)
        in valid t' && lookup k t' == Just expect

prop_unsafeInsertWithLargerThanMaxPrioView :: Property
prop_unsafeInsertWithLargerThanMaxPrioView =
    forAll arbitraryPSQ $ \t0 ->
    forAll arbitrary    $ \k  ->
    forAll arbitrary    $ \x  ->
        let t          = fmap (\e -> [e]) t0 :: IntPSQ Int [Char]
            prio       = largerThanMaxPrio t
            f          = \newP newX oldP oldX ->
                            (newP + (abs oldP `div` 2), newX ++ oldX)
            (mbPx, t') = unsafeInsertWithLargerThanMaxPrioView f k prio [x] t
            expect     = case mbPx of
                            Nothing     -> (prio, [x])
                            Just (p, y) -> (prio + (abs p `div` 2), [x] ++ y)
        in valid t' &&
            lookup k t' == Just expect &&
            lookup k t  == mbPx
