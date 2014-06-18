-- | Various test utilities
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.PSQ.Tests.Util
    ( LousyHashedInt (..)
    , TestKey (..)
    , arbitraryInt
    , arbitraryPriority
    , arbitraryTestKey
    , coverShowInstance
    , assertErrorCall
    ) where

import           Control.Applicative ((<$>))
import           Data.Hashable       (Hashable (..))
import           Control.Exception   (ErrorCall (..), fromException, handle)
import           Test.HUnit          (Assertion, assertFailure)
import           Test.QuickCheck     (Arbitrary (..), Gen, arbitrary, choose)
import           Control.DeepSeq     (NFData)

-- | A type we used a key in the PSQs in the tests. It intentionally has a
-- really bad 'Hashable' instance so we get lots of collisions.
newtype LousyHashedInt = LousyHashedInt Int
    deriving (Enum, Eq, Integral, NFData, Num, Ord, Real, Show)

instance Arbitrary LousyHashedInt where
    arbitrary = LousyHashedInt <$> arbitraryInt

instance Hashable LousyHashedInt where
    hashWithSalt salt (LousyHashedInt x) = hashWithSalt salt x `mod` 100

class (Arbitrary a, Enum a, Eq a, Num a, Ord a, Show a) => TestKey a where
    toTestKey :: Int -> a
    toTestKey = toEnum

    fromTestKey :: a -> Int
    fromTestKey = fromEnum

instance TestKey LousyHashedInt where

instance TestKey Int where

arbitraryInt :: Gen Int
arbitraryInt = arbitrary

-- | Makes sure the priorities are taken from a small set so we have some
-- collisions.
arbitraryPriority :: Gen Int
arbitraryPriority = choose (-10, 10)

arbitraryTestKey :: TestKey a => Gen a
arbitraryTestKey = toEnum <$> arbitraryInt

-- | This is a bit ridiculous. We need to call all 'Show' methods to get 100%
-- coverage.
coverShowInstance :: Show a => a -> String
coverShowInstance x =
    showsPrec 0 x $
    showList [x]  $
    show x

-- | Check that evaluating the second argument to Whitney Houston Normal Form
-- results in a call to `error`. The error message is passed to the first
-- handler, so you can perform checks on it.
assertErrorCall :: (String -> Assertion) -> a -> Assertion
assertErrorCall handler x = handle
    (\e -> case fromException e of
            Just (ErrorCall str) -> handler str
            Nothing              -> assertFailure $
                "assertErrorCall: expected `error` but got: " ++ show e)
    (x `seq` assertFailure
        "assertErrorCall: evaluated to WHNF and no exception was thrown")
