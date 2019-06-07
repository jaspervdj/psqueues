module Data.HashPSQ.Tests
    ( tests
    ) where

import           Prelude               hiding (lookup)

import           Test.HUnit            (Assertion, assert)
import           Test.QuickCheck       (Property, arbitrary, forAll)
import           Test.Tasty            (TestTree)
import           Test.Tasty.HUnit      (testCase)
import           Test.Tasty.QuickCheck (testProperty)

import           Data.HashPSQ.Internal
import qualified Data.OrdPSQ           as OrdPSQ
import           Data.PSQ.Class.Gen
import           Data.PSQ.Class.Util


--------------------------------------------------------------------------------
-- Index of tests
--------------------------------------------------------------------------------

tests :: [TestTree]
tests =
    [ testCase      "showBucket"    test_showBucket
    , testCase      "toBucket"      test_toBucket
    , testProperty "unsafeLookupIncreasePriority"
                                    prop_unsafeLookupIncreasePriority
    , testProperty "unsafeInsertIncreasePriority"
                                    prop_unsafeInsertIncreasePriority
    , testProperty "unsafeInsertIncreasePriorityView"
                                    prop_unsafeInsertIncreasePriorityView
    ]


--------------------------------------------------------------------------------
-- Unit tests
--------------------------------------------------------------------------------

test_showBucket :: Assertion
test_showBucket =
    assert $ length (coverShowInstance bucket) > 0
  where
    bucket :: Bucket Int Int Char
    bucket = B 1 'a' OrdPSQ.empty

test_toBucket :: Assertion
test_toBucket =
    assert True
    -- TODO (jaspervdj)
    -- assert $ mkBucket (OrdPSQ.empty :: OrdPSQ.OrdPSQ Int Int Char)


--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

prop_unsafeLookupIncreasePriority :: Property
prop_unsafeLookupIncreasePriority =
    forAll arbitraryPSQ $ \t ->
    forAll arbitrary    $ \k  ->
        let newP       = maybe 0 ((+ 1) . fst) (lookup k t)
            (mbPx, t') = unsafeLookupIncreasePriority k newP t
            expect     = case mbPx of
                Nothing     -> Nothing
                Just (p, x) -> Just (p + 1, x)
        in valid (t' :: HashPSQ LousyHashedInt Int Char) &&
            lookup k t' == expect &&
            lookup k t  == mbPx

prop_unsafeInsertIncreasePriority :: Property
prop_unsafeInsertIncreasePriority =
    forAll arbitraryPSQ $ \t ->
    forAll arbitrary    $ \k ->
    forAll arbitrary    $ \x ->
        let prio = largerThanMaxPrio t
            t'   = unsafeInsertIncreasePriority k prio x t
        in valid (t' :: HashPSQ LousyHashedInt Int Char) &&
            lookup k t' == Just (prio, x)

prop_unsafeInsertIncreasePriorityView :: Property
prop_unsafeInsertIncreasePriorityView =
    forAll arbitraryPSQ $ \t ->
    forAll arbitrary    $ \k ->
    forAll arbitrary    $ \x ->
        let prio       = largerThanMaxPrio t
            (mbPx, t') = unsafeInsertIncreasePriorityView k prio x t
        in valid (t' :: HashPSQ LousyHashedInt Int Char) &&
            lookup k t' == Just (prio, x) &&
            lookup k t  == mbPx
