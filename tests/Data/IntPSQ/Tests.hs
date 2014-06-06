module Data.IntPSQ.Tests
    ( tests
    ) where

import           Prelude             hiding (null, lookup)
import           Control.Applicative ((<$>))

import           Test.HUnit                           (Assertion, (@?=))
import           Test.Framework                       (Test)
import           Test.Framework.Providers.HUnit       (testCase)

import           Data.IntPSQ

tests :: [Test]
tests =
    [ testCase "size"   test_size
    , testCase "size2"  test_size2
    , testCase "empty"  test_empty
    , testCase "lookup" test_lookup
    ]

type UIntPSQ = IntPSQ Int ()
type CIntPSQ = IntPSQ Int Char
type IIntPSQ = IntPSQ Int Int

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
