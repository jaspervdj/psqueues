module Data.IntPSQ.Tests
    where

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
    , testProperty "unsafeInsertIncreasedPriority"
                                    prop_unsafeInsertIncreasedPriority
    , testProperty "unsafeInsertIncreasedPriorityView"
                                    prop_unsafeInsertIncreasedPriorityView
    , testProperty "unsafeInsertWithIncreasedPriority"
                                    prop_unsafeInsertWithIncreasedPriority
    , testProperty "unsafeInsertWithIncreasedPriorityView"
                                    prop_unsafeInsertWithIncreasedPriorityView
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

prop_unsafeInsertIncreasedPriority :: Property
prop_unsafeInsertIncreasedPriority =
    forAll arbitraryPSQ $ \t ->
    forAll arbitrary    $ \k ->
    forAll arbitrary    $ \x ->
        let prio = largerThanMaxPrio t
            t'   = unsafeInsertIncreasedPriority k prio x t
        in valid (t' :: IntPSQ Int Char) && lookup k t' == Just (prio, x)

prop_unsafeInsertIncreasedPriorityView :: Property
prop_unsafeInsertIncreasedPriorityView =
    forAll arbitraryPSQ $ \t ->
    forAll arbitrary    $ \k ->
    forAll arbitrary    $ \x ->
        let prio       = largerThanMaxPrio t
            (mbPx, t') = unsafeInsertIncreasedPriorityView k prio x t
        in valid (t' :: IntPSQ Int Char) &&
            lookup k t' == Just (prio, x) &&
            lookup k t  == mbPx

prop_unsafeInsertWithIncreasedPriority :: Property
prop_unsafeInsertWithIncreasedPriority =
    forAll arbitraryPSQ $ \t0 ->
    forAll arbitrary    $ \k  ->
    forAll arbitrary    $ \x  ->
        let t      = fmap (\e -> [e]) t0 :: IntPSQ Int [Char]
            prio   = largerThanMaxPrio t
            f      = \newP newX oldP oldX ->
                        (min newP oldP + 1, newX ++ oldX)
            t'     = unsafeInsertWithIncreasedPriority f k prio [x] t
            expect = case lookup k t of
                            Nothing     -> (prio, [x])
                            Just (p, y) -> (min prio p + 1, [x] ++ y)
        in valid t' && lookup k t' == Just expect

prop_unsafeInsertWithIncreasedPriorityView :: Property
prop_unsafeInsertWithIncreasedPriorityView =
    forAll arbitraryPSQ $ \t0 ->
    forAll arbitrary    $ \k  ->
    forAll arbitrary    $ \x  ->
        let t          = fmap (\e -> [e]) t0 :: IntPSQ Int [Char]
            prio       = largerThanMaxPrio t
            f          = \newP newX oldP oldX ->
                            (min newP oldP + 1, newX ++ oldX)
            (mbPx, t') = unsafeInsertWithIncreasedPriorityView f k prio [x] t
            expect     = case mbPx of
                            Nothing     -> (prio, [x])
                            Just (p, y) -> (min prio p + 1, [x] ++ y)
        in valid t' &&
            lookup k t' == Just expect &&
            lookup k t  == mbPx
