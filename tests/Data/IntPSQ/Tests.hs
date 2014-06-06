{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.IntPSQ.Tests
    ( tests
    ) where

import           Prelude             hiding (null, lookup)
import           Control.Applicative ((<$>))

import           Test.QuickCheck                      (Arbitrary (..), Property,
                                                       (==>))
import           Test.HUnit                           (Assertion, (@?=))
import           Test.Framework                       (Test)
import           Test.Framework.Providers.HUnit       (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)

import           Data.IntPSQ

--------------------------------------------------------------------------------
-- Index of tests
--------------------------------------------------------------------------------

tests :: [Test]
tests =
    [ testCase "size"   test_size
    , testCase "size2"  test_size2
    , testCase "empty"  test_empty
    , testCase "lookup" test_lookup

    , testProperty "prop_singleton"       prop_singleton
    , testProperty "prop_insertLookup"    prop_insertLookup
    , testProperty "prop_insertDelete"    prop_insertDelete
    , testProperty "prop_deleteNonMember" prop_deleteNonMember
    ]

--------------------------------------------------------------------------------
-- Arbitrary instance
--------------------------------------------------------------------------------

instance (Arbitrary p, Arbitrary v, Ord p) => Arbitrary (IntPSQ p v) where
    arbitrary = fromList <$> arbitrary

--------------------------------------------------------------------------------
-- Convenient type shorthands
--------------------------------------------------------------------------------

type UIntPSQ = IntPSQ Int ()
type CIntPSQ = IntPSQ Int Char
type IIntPSQ = IntPSQ Int Int

--------------------------------------------------------------------------------
-- HUnit tests
--------------------------------------------------------------------------------

test_size :: Assertion
test_size = do
    null (empty)                          @?= True
    null (singleton 1 100 'a' :: CIntPSQ) @?= False

test_size2 :: Assertion
test_size2 = do
    size empty                            @?= 0
    size (singleton 1 100 'a' :: CIntPSQ) @?= 1
    size (fromList ([(1, 100, 'a'), (2, 101, 'c'), (3, 102, 'b')]) :: CIntPSQ)
                                          @?= 3

test_empty :: Assertion
test_empty = do
    toList (empty :: UIntPSQ) @?= []
    size empty @?= 0

test_lookup :: Assertion
test_lookup = do
    employeeCurrency 1 @?= Just 1
    employeeCurrency 2 @?= Nothing
  where
    employeeDept    = fromList [(1, 100, 2), (3, 101, 1)] :: IIntPSQ
    deptCountry     = fromList [(1, 102, 1), (2, 103, 2)] :: IIntPSQ
    countryCurrency = fromList [(1, 104, 2), (2, 105, 1)] :: IIntPSQ

    employeeCurrency :: Int -> Maybe Int
    employeeCurrency name = do
        dept    <- snd <$> lookup name employeeDept
        country <- snd <$> lookup dept deptCountry
        snd <$> lookup country countryCurrency

--------------------------------------------------------------------------------
-- QuickCheck properties
--------------------------------------------------------------------------------

prop_singleton :: Int -> Int -> String -> Bool
prop_singleton k p x = insert k p x empty == singleton k p x

prop_insertLookup :: Int -> Int -> UIntPSQ -> Bool
prop_insertLookup k p t = lookup k (insert k p () t) /= Nothing

prop_insertDelete :: Int -> Int -> UIntPSQ -> Property
prop_insertDelete k p t =
    (lookup k t == Nothing) ==> (delete k (insert k p () t) == t)

prop_deleteNonMember :: Int -> UIntPSQ -> Property
prop_deleteNonMember k t = (lookup k t == Nothing) ==> (delete k t == t)
