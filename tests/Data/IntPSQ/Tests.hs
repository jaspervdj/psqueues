module Data.IntPSQ.Tests
    ( tests
    ) where

import           Prelude   hiding (null)

import           Test.HUnit                           (Assertion, (@?=))
import           Test.Framework                       (Test)
import           Test.Framework.Providers.HUnit       (testCase)

import           Data.IntPSQ

tests :: [Test]
tests =
    [ testCase "size" test_size
    , testCase "size2" test_size2
    , testCase "empty" test_empty
    ]

type UIntPSQ = IntPSQ Int ()
type CIntPSQ = IntPSQ Int Char

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
