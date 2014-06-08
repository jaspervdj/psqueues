module Data.PSQ.Tests
    ( tests
    ) where

import           Test.Framework                       (Test)
import           Test.Framework.Providers.QuickCheck2 (testProperty)

import           Data.PSQ
import           Data.PSQ.Class.Gen                   ()

--------------------------------------------------------------------------------
-- Index of tests
--------------------------------------------------------------------------------

tests :: [Test]
tests =
    [ testProperty "toAscList" prop_toAscList
    ]


--------------------------------------------------------------------------------
-- QuickCheck properties
--------------------------------------------------------------------------------

prop_toAscList :: PSQ Int Int Char -> Bool
prop_toAscList t = isUniqueSorted [k | (k, _, _) <- toAscList t]
  where
    isUniqueSorted (x : y : zs) = x < y && isUniqueSorted (y : zs)
    isUniqueSorted [_]          = True
    isUniqueSorted []           = True
