-- | Higher-quality random generator for PSQ structures which generates a PSQ
-- from a series of actions
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.PSQ.Class.Gen
    ( arbitraryPSQ
    ) where

import           Control.Applicative (pure, (<$>), (<*>))
import           Test.QuickCheck     (Gen, Arbitrary (..), frequency, choose,
                                      elements)
import           Control.Monad       (foldM, replicateM)
import           Data.Hashable       (Hashable)

import           Data.PSQ.Class      (PSQ (..))
import           Data.PSQ.Class.Util
import qualified Data.OrdPSQ         as OrdPSQ
import qualified Data.IntPSQ         as IntPSQ
import qualified Data.HashPSQ        as HashPSQ

data Action k p v
    = Insert k p v
    | DeleteRandomMember
    | DeleteMin
    deriving (Show, Eq)

arbitraryAction
    :: (Arbitrary k, Arbitrary v)
    => Gen (Action k Int v)
arbitraryAction = frequency
    [ (10, Insert <$> arbitrary <*> arbitraryPriority <*> arbitrary)
    , (2,  pure DeleteRandomMember)
    , (2,  pure DeleteMin)
    ]

apply
    :: PSQ psq => Action (Key psq) Int v -> psq Int v -> Gen (psq Int v)
apply (Insert k p x)     t = return $ insert k p x t
apply DeleteRandomMember t = do
    key <- elements (keys t)
    return $ delete key t
apply DeleteMin          t = return $ case minView t of
    Nothing            -> t
    Just (_, _, _, t') -> t'

arbitraryPSQ
    :: forall psq v. (Arbitrary (Key psq), Arbitrary v, PSQ psq)
    => Gen (psq Int v)
arbitraryPSQ = do
    numActions <- choose (0, 100)
    actions    <- replicateM numActions arbitraryAction
    foldM (\t a -> apply a t) (empty :: psq Int v) actions

shrinkPSQ
    :: forall psq p v. (Ord p, PSQ psq)
    => psq p v -> [psq p v]
shrinkPSQ t = [delete k t | k <- keys t]

instance forall k v. (Arbitrary k, Arbitrary v, Ord k) =>
            Arbitrary (OrdPSQ.OrdPSQ k Int v) where
    arbitrary = arbitraryPSQ
    shrink    = shrinkPSQ

instance forall v. (Arbitrary v) => Arbitrary (IntPSQ.IntPSQ Int v) where
    arbitrary = arbitraryPSQ
    shrink    = shrinkPSQ

instance forall k v. (Arbitrary k, Arbitrary v,
                      Hashable k, Ord k) =>
            Arbitrary (HashPSQ.HashPSQ k Int v) where
    arbitrary = arbitraryPSQ
    shrink    = shrinkPSQ
