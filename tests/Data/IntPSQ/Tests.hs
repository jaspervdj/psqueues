module Data.IntPSQ.Tests
    where

import           Prelude               hiding (lookup)

import           Test.HUnit            (Assertion, assert)
import           Test.QuickCheck       (Property, arbitrary, forAll)
import           Test.Tasty            (TestTree)
import           Test.Tasty.HUnit      (testCase)
import           Test.Tasty.QuickCheck (testProperty)

import           Data.IntPSQ.Internal
import           Data.PSQ.Class.Gen
import           Data.PSQ.Class.Util

--------------------------------------------------------------------------------
-- Index of tests
--------------------------------------------------------------------------------

tests :: [TestTree]
tests =
    [ testCase     "hasBadNils"     test_hasBadNils
    , testProperty "unsafeInsertIncreasePriority"
                                    prop_unsafeInsertIncreasePriority
    , testProperty "unsafeInsertIncreasePriorityView"
                                    prop_unsafeInsertIncreasePriorityView
    , testProperty "unsafeInsertWithIncreasePriority"
                                    prop_unsafeInsertWithIncreasePriority
    , testProperty "unsafeInsertWithIncreasePriorityView"
                                    prop_unsafeInsertWithIncreasePriorityView
    , testProperty "unsafeLookupIncreasePriority"
                                    prop_unsafeLookupIncreasePriority
    ]


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

prop_unsafeInsertIncreasePriority :: Property
prop_unsafeInsertIncreasePriority =
    forAll arbitraryPSQ $ \t ->
    forAll arbitrary    $ \k ->
    forAll arbitrary    $ \x ->
        let prio = largerThanMaxPrio t
            t'   = unsafeInsertIncreasePriority k prio x t
        in valid (t' :: IntPSQ Int Char) && lookup k t' == Just (prio, x)

prop_unsafeInsertIncreasePriorityView :: Property
prop_unsafeInsertIncreasePriorityView =
    forAll arbitraryPSQ $ \t ->
    forAll arbitrary    $ \k ->
    forAll arbitrary    $ \x ->
        let prio       = largerThanMaxPrio t
            (mbPx, t') = unsafeInsertIncreasePriorityView k prio x t
        in valid (t' :: IntPSQ Int Char) &&
            lookup k t' == Just (prio, x) &&
            lookup k t  == mbPx

prop_unsafeInsertWithIncreasePriority :: Property
prop_unsafeInsertWithIncreasePriority =
    forAll arbitraryPSQ $ \t0 ->
    forAll arbitrary    $ \k  ->
    forAll arbitrary    $ \x  ->
        let t      = fmap (\e -> [e]) t0 :: IntPSQ Int [Char]
            prio   = largerThanMaxPrio t
            f      = \newP newX oldP oldX ->
                        (min newP oldP + 1, newX ++ oldX)
            t'     = unsafeInsertWithIncreasePriority f k prio [x] t
            expect = case lookup k t of
                            Nothing     -> (prio, [x])
                            Just (p, y) -> (min prio p + 1, [x] ++ y)
        in valid t' && lookup k t' == Just expect

prop_unsafeInsertWithIncreasePriorityView :: Property
prop_unsafeInsertWithIncreasePriorityView =
    forAll arbitraryPSQ $ \t0 ->
    forAll arbitrary    $ \k  ->
    forAll arbitrary    $ \x  ->
        let t          = fmap (\e -> [e]) t0 :: IntPSQ Int [Char]
            prio       = largerThanMaxPrio t
            f          = \newP newX oldP oldX ->
                            (min newP oldP + 1, newX ++ oldX)
            (mbPx, t') = unsafeInsertWithIncreasePriorityView f k prio [x] t
            expect     = case mbPx of
                            Nothing     -> (prio, [x])
                            Just (p, y) -> (min prio p + 1, [x] ++ y)
        in valid t' &&
            lookup k t' == Just expect &&
            lookup k t  == mbPx

prop_unsafeLookupIncreasePriority :: Property
prop_unsafeLookupIncreasePriority =
    forAll arbitraryPSQ $ \t0 ->
    forAll arbitrary    $ \k  ->
        let t          = fmap (\e -> [e]) t0 :: IntPSQ Int [Char]
            f          = \oldP oldX ->
                            (Just (oldP, oldX), oldP + 1, oldX ++ "k")
            (mbPx, t') = unsafeLookupIncreasePriority f k t
            expect     = case mbPx of
                Nothing     -> Nothing
                Just (p, x) -> Just (p + 1, x ++ "k")
        in valid t' &&
            lookup k t' == expect &&
            lookup k t  == mbPx
