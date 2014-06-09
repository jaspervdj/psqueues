-- | Various test utilities
module Data.PSQ.Tests.Util
    ( arbitraryInt
    , coverShowInstance
    , assertErrorCall
    ) where

import           Control.Exception (ErrorCall (..), fromException, handle)
import           Test.HUnit        (Assertion, assertFailure)
import           Test.QuickCheck   (Gen, arbitrary)

-- | Useful because we don't have to fix the type.
arbitraryInt :: Gen Int
arbitraryInt = arbitrary

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
