module Data.HashPSQ.Tests
    ( tests
    ) where

import           Prelude                        hiding (lookup)

import           Test.Framework                       (Test)
import           Test.Framework.Providers.HUnit       (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck                      (Property, arbitrary,
                                                       forAll)
import           Test.HUnit                           (Assertion, assert)

import           Data.HashPSQ.Internal
import qualified Data.OrdPSQ                    as OrdPSQ
import           Data.PSQ.Class.Gen
import           Data.PSQ.Class.Util


--------------------------------------------------------------------------------
-- Index of tests
--------------------------------------------------------------------------------

tests :: [Test]
tests =
    [ testCase "showBucket" test_showBucket
    , testCase "toBucket"   test_toBucket
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
