module Data.HashPSQ.Tests
    ( tests
    ) where

import           Prelude                        hiding (lookup)

import           Data.Maybe                     (isNothing)
import           Test.Framework                 (Test)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assert)

import           Data.HashPSQ.Internal
import qualified Data.PSQ                       as PSQ
import           Data.PSQ.Tests.Util

--------------------------------------------------------------------------------
-- Index of tests
--------------------------------------------------------------------------------

tests :: [Test]
tests =
    [ testCase "showBucket" test_showBucket
    , testCase "toBucket"   test_toBucket
    ]


--------------------------------------------------------------------------------
-- Unit tests
--------------------------------------------------------------------------------

test_showBucket :: Assertion
test_showBucket =
    assert $ length (coverShowInstance bucket) > 0
  where
    bucket :: Bucket Int Int Char
    bucket = B 1 'a' PSQ.empty

test_toBucket :: Assertion
test_toBucket =
    assert $ isNothing $ toBucket (PSQ.empty :: PSQ.PSQ Int Int Char)
