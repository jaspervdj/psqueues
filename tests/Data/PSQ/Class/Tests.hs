{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.PSQ.Class.Tests
    ( tests
    ) where

import           Prelude             hiding (null, lookup)
import           Control.Applicative ((<$>))
import           Data.Tagged         (Tagged (..), untag)

import           Test.QuickCheck                      (Arbitrary (..), Property,
                                                       Gen, (==>), forAll)
import           Test.HUnit                           (Assertion, (@?=))
import           Test.Framework                       (Test)
import           Test.Framework.Providers.HUnit       (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)

import           Data.PSQ.Class

--------------------------------------------------------------------------------
-- Index of tests
--------------------------------------------------------------------------------

tests
    :: forall psq. (PSQ psq, Eq (psq Int Char), Show (psq Int Char))
    => Tagged psq [Test]
tests = Tagged
    [ testCase "size"   (untag' test_size)
    , testCase "size2"  (untag' test_size2)
    , testCase "empty"  (untag' test_empty)
    , testCase "lookup" (untag' test_lookup)

    , testProperty "prop_singleton"       (untag' prop_singleton)
    , testProperty "prop_insertLookup"    (untag' prop_insertLookup)
    , testProperty "prop_insertDelete"    (untag' prop_insertDelete)
    , testProperty "prop_deleteNonMember" (untag' prop_deleteNonMember)
    ]
  where
    untag' :: Tagged psq test -> test
    untag' = untag


--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

arbitraryInt :: Gen Int
arbitraryInt = arbitrary

arbitraryPSQ :: (Arbitrary p, Arbitrary v, Ord p, PSQ psq) => Gen (psq p v)
arbitraryPSQ = fromList <$> arbitrary


--------------------------------------------------------------------------------
-- HUnit tests
--------------------------------------------------------------------------------

test_size :: forall psq. PSQ psq => Tagged psq Assertion
test_size = Tagged $ do
    null (empty               :: psq Int Char) @?= True
    null (singleton 1 100 'a' :: psq Int Char) @?= False

test_size2 :: forall psq. PSQ psq => Tagged psq Assertion
test_size2 = Tagged $ do
    size (empty               :: psq Int ())   @?= 0
    size (singleton 1 100 'a' :: psq Int Char) @?= 1
    size (fromList [(1, 100, 'a'), (2, 101, 'c'), (3, 102, 'b')]
                              :: psq Int Char) @?= 3

test_empty :: forall psq. PSQ psq => Tagged psq Assertion
test_empty = Tagged $ do
    toList (empty :: psq Int ())   @?= []
    size   (empty :: psq Char Int) @?= 0

test_lookup :: forall psq. PSQ psq => Tagged psq Assertion
test_lookup = Tagged $ do
    employeeCurrency 1 @?= Just 1
    employeeCurrency 2 @?= Nothing
  where
    employeeDept    = fromList [(1, 100, 2), (3, 101, 1)] :: psq Int Int
    deptCountry     = fromList [(1, 102, 1), (2, 103, 2)] :: psq Int Int
    countryCurrency = fromList [(1, 104, 2), (2, 105, 1)] :: psq Int Int

    employeeCurrency :: Int -> Maybe Int
    employeeCurrency name = do
        dept    <- snd <$> lookup name employeeDept
        country <- snd <$> lookup dept deptCountry
        snd <$> lookup country countryCurrency


--------------------------------------------------------------------------------
-- QuickCheck properties
--------------------------------------------------------------------------------

prop_singleton
    :: forall psq. (Eq (psq Int Char), PSQ psq)
    => Tagged psq (Int -> Int -> Char -> Bool)
prop_singleton = Tagged $ \k p x ->
    insert k p x empty == (singleton k p x :: psq Int Char)

prop_insertLookup
    :: forall psq. (PSQ psq, Show (psq Int Char))
    => Tagged psq Property
prop_insertLookup = Tagged $
    forAll arbitraryInt $ \k ->
    forAll arbitraryInt $ \p ->
    forAll arbitrary    $ \c ->
    forAll arbitraryPSQ $ \t ->
        lookup k (insert k p c (t :: psq Int Char)) /= Nothing

prop_insertDelete
    :: forall psq. (PSQ psq, Eq (psq Int Char), Show (psq Int Char))
    => Tagged psq Property
prop_insertDelete = Tagged $
    forAll arbitraryInt $ \k ->
    forAll arbitraryInt $ \p ->
    forAll arbitrary    $ \c ->
    forAll arbitraryPSQ $ \t ->
        (lookup k t == Nothing) ==>
            (delete k (insert k p c t) == (t :: psq Int Char))

prop_deleteNonMember
    :: forall psq. (PSQ psq, Eq (psq Int Char), Show (psq Int Char))
    => Tagged psq Property
prop_deleteNonMember = Tagged $
    forAll arbitraryInt $ \k ->
    forAll arbitraryPSQ $ \t ->
        (lookup k t == Nothing) ==> (delete k t == (t :: psq Int Char))
