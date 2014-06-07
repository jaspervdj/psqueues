{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.PSQ.Class.Tests
    ( tests
    ) where

import           Prelude             hiding (null, lookup)
import           Control.Applicative ((<$>))
import           Data.Tagged         (Tagged (..), untag)
import qualified Data.List           as List

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
    :: forall psq. (PSQ psq, Key psq ~ Int,
                    Eq (psq Int Char), Show (psq Int Char))
    => Tagged psq [Test]
tests = Tagged
    [ testCase "size"    (untag' test_size)
    , testCase "size2"   (untag' test_size2)
    , testCase "empty"   (untag' test_empty)
    , testCase "lookup"  (untag' test_lookup)
    , testCase "findMin" (untag' test_findMin)
    , testCase "alter"   (untag' test_alter)

    , testProperty "singleton"       (untag' prop_singleton)
    , testProperty "insertLookup"    (untag' prop_insertLookup)
    , testProperty "insertDelete"    (untag' prop_insertDelete)
    , testProperty "deleteNonMember" (untag' prop_deleteNonMember)
    , testProperty "alter"           (untag' prop_alter)
    , testProperty "toList"          (untag' prop_toList)
    , testProperty "fold'"           (untag' prop_fold')
    ]
  where
    untag' :: Tagged psq test -> test
    untag' = untag


--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

arbitraryInt :: Gen Int
arbitraryInt = arbitrary

arbitraryPSQ
    :: (Arbitrary p, Arbitrary v, Arbitrary (Key psq), Ord p, PSQ psq)
    => Gen (psq p v)
arbitraryPSQ = fromList <$> arbitrary


--------------------------------------------------------------------------------
-- HUnit tests
--------------------------------------------------------------------------------

test_size
    :: forall psq. (PSQ psq, Key psq ~ Int)
    => Tagged psq Assertion
test_size = Tagged $ do
    null (empty               :: psq Int Char) @?= True
    null (singleton 1 100 'a' :: psq Int Char) @?= False

test_size2
    :: forall psq. (PSQ psq, Key psq ~ Int)
    => Tagged psq Assertion
test_size2 = Tagged $ do
    size (empty               :: psq Int ())   @?= 0
    size (singleton 1 100 'a' :: psq Int Char) @?= 1
    size (fromList [(1, 100, 'a'), (2, 101, 'c'), (3, 102, 'b')]
                              :: psq Int Char) @?= 3

test_empty
    :: forall psq. (PSQ psq, Key psq ~ Int)
    => Tagged psq Assertion
test_empty = Tagged $ do
    toList (empty :: psq Int ())   @?= []
    size   (empty :: psq Char Int) @?= 0

test_lookup
    :: forall psq. (PSQ psq, Key psq ~ Int)
    => Tagged psq Assertion
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

test_findMin
    :: forall psq. (PSQ psq, Key psq ~ Int)
    => Tagged psq Assertion
test_findMin = Tagged $ do
    findMin (empty :: psq Int Char) @?= Nothing
    findMin (fromList [(5, 101, 'a'), (3, 100, 'b')] :: psq Int Char) @?=
        Just (3, 100, 'b')

test_alter
    :: forall psq. (PSQ psq, Key psq ~ Int,
                    Eq (psq Int Char), Show (psq Int Char))
    => Tagged psq Assertion
test_alter = Tagged $
    alter f 3 (empty :: psq Int Char) @?= ("Hello", singleton 3 100 'a')
  where
    f _ = ("Hello", Just (100, 'a'))


--------------------------------------------------------------------------------
-- QuickCheck properties
--------------------------------------------------------------------------------

prop_singleton
    :: forall psq. (PSQ psq, Key psq ~ Int,
                    Eq (psq Int Char))
    => Tagged psq (Int -> Int -> Char -> Bool)
prop_singleton = Tagged $ \k p x ->
    insert k p x empty == (singleton k p x :: psq Int Char)

prop_insertLookup
    :: forall psq. (PSQ psq, Key psq ~ Int,
                    Show (psq Int Char))
    => Tagged psq Property
prop_insertLookup = Tagged $
    forAll arbitraryInt $ \k ->
    forAll arbitraryInt $ \p ->
    forAll arbitrary    $ \c ->
    forAll arbitraryPSQ $ \t ->
        lookup k (insert k p c (t :: psq Int Char)) /= Nothing

prop_insertDelete
    :: forall psq. (PSQ psq, Key psq ~ Int,
                    Eq (psq Int Char), Show (psq Int Char))
    => Tagged psq Property
prop_insertDelete = Tagged $
    forAll arbitraryInt $ \k ->
    forAll arbitraryInt $ \p ->
    forAll arbitrary    $ \c ->
    forAll arbitraryPSQ $ \t ->
        (lookup k t == Nothing) ==>
            (delete k (insert k p c t) == (t :: psq Int Char))

prop_deleteNonMember
    :: forall psq. (PSQ psq, Key psq ~ Int,
                    Eq (psq Int Char), Show (psq Int Char))
    => Tagged psq Property
prop_deleteNonMember = Tagged $
    forAll arbitraryInt $ \k ->
    forAll arbitraryPSQ $ \t ->
        (lookup k t == Nothing) ==> (delete k t == (t :: psq Int Char))

prop_alter
    :: forall psq. (PSQ psq, Key psq ~ Int, Show (psq Int Char))
    => Tagged psq Property
prop_alter = Tagged $
    forAll arbitraryInt $ \k ->
    forAll arbitraryPSQ $ \t ->
        let ((), t') = alter f k t :: ((), psq Int Char)
        in case lookup k t of
            Just _  -> (size t - 1) == size t' && lookup k t' == Nothing
            Nothing -> (size t + 1) == size t' && lookup k t' /= Nothing
  where
    f Nothing   = ((), Just (100, 'a'))
    f (Just _)  = ((), Nothing)

prop_toList
    :: forall psq. (PSQ psq, Key psq ~ Int,
                    Eq (psq Int Char), Show (psq Int Char))
    => Tagged psq Property
prop_toList = Tagged $
    forAll arbitraryPSQ $ \t ->
        (t :: psq Int Char) == fromList (toList t)

prop_fold'
    :: forall psq. (PSQ psq, Key psq ~ Int, Show (psq Int Char))
    => Tagged psq Property
prop_fold' = Tagged $
    forAll arbitraryPSQ $ \t ->
        fold' f acc0 (t :: psq Int Char) ==
            List.foldl' (\acc (k, p, x) -> f k p x acc) acc0 (toList t)
  where
    -- Needs to be commutative
    f k p x (kpSum, xs) = (kpSum + k + p, List.sort (x : xs))
    acc0                = (0, [])
