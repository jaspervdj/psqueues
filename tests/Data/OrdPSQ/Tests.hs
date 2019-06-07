module Data.OrdPSQ.Tests
    ( tests
    ) where

import           Data.List             (isInfixOf)
import           Test.HUnit            (Assertion, assert)
import           Test.Tasty            (TestTree)
import           Test.Tasty.HUnit      (testCase)
import           Test.Tasty.QuickCheck (testProperty)

import           Data.OrdPSQ.Internal
import           Data.PSQ.Class.Gen    ()
import           Data.PSQ.Class.Util

--------------------------------------------------------------------------------
-- Index of tests
--------------------------------------------------------------------------------

tests :: [TestTree]
tests =
    [ testCase     "showElem"      test_showElem
    , testCase     "showLTree"     test_showLTree
    , testCase     "invalidLTree"  test_invalidLTree
    , testCase     "balanceErrors" test_balanceErrors
    , testProperty "toAscList"     prop_toAscList
    ]


--------------------------------------------------------------------------------
-- Tests the result of 'moduleError' for internal issues
--------------------------------------------------------------------------------

assertModuleError :: String -> String -> a -> Assertion
assertModuleError fun msg = assertErrorCall $ \e -> do
    assert $ fun `isInfixOf` e
    assert $ msg `isInfixOf` e


--------------------------------------------------------------------------------
-- HUnit tests
--------------------------------------------------------------------------------

test_showElem :: Assertion
test_showElem =
    assert $ length (coverShowInstance (E 0 0 'A' :: Elem Int Int Char)) > 0

test_showLTree :: Assertion
test_showLTree = do
    assert $ length (coverShowInstance t1) > 0
    assert $ length (coverShowInstance t2) > 0
    assert $ length (coverShowInstance t3) > 0
  where
    t1, t2, t3 :: LTree Int Int Char
    t1 = Start
    t2 = LLoser 1 e Start 0 Start
    t3 = RLoser 1 e Start 0 Start
    e  = E 0 0 'A'

test_invalidLTree :: Assertion
test_invalidLTree = do
    assertModuleError "left"   "empty" (left   (Start :: LTree Int Int Char))
    assertModuleError "right"  "empty" (right  (Start :: LTree Int Int Char))
    assertModuleError "maxKey" "empty" (maxKey (empty :: OrdPSQ Int Int Char))

test_balanceErrors :: Assertion
test_balanceErrors = do
    assertModuleError "lsingleLeft"  msg (lsingleLeft  0 0 'A' nil 0 nil)
    assertModuleError "rsingleLeft"  msg (rsingleLeft  0 0 'A' nil 0 nil)
    assertModuleError "lsingleRight" msg (lsingleRight 0 0 'A' nil 0 nil)
    assertModuleError "rsingleRight" msg (rsingleRight 0 0 'A' nil 0 nil)
    assertModuleError "ldoubleLeft"  msg (ldoubleLeft  0 0 'A' nil 0 nil)
    assertModuleError "rdoubleLeft"  msg (rdoubleLeft  0 0 'A' nil 0 nil)
    assertModuleError "ldoubleRight" msg (ldoubleRight 0 0 'A' nil 0 nil)
    assertModuleError "rdoubleRight" msg (rdoubleRight 0 0 'A' nil 0 nil)
  where
    nil = Start :: LTree Int Int Char
    msg = "malformed"


--------------------------------------------------------------------------------
-- QuickCheck properties
--------------------------------------------------------------------------------

prop_toAscList :: OrdPSQ Int Int Char -> Bool
prop_toAscList t = isUniqueSorted [k | (k, _, _) <- toAscList t]
  where
    isUniqueSorted (x : y : zs) = x < y && isUniqueSorted (y : zs)
    isUniqueSorted [_]          = True
    isUniqueSorted []           = True
