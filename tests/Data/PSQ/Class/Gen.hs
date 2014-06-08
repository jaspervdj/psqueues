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
import qualified Data.PSQ            as PSQ
import qualified Data.IntPSQ         as IntPSQ
import qualified Data.HashPSQ        as HashPSQ

data Action k p v
    = Insert k p v
    | DeleteRandomMember
    | DeleteMin
    deriving (Show, Eq)

arbitraryAction
    :: (Arbitrary k, Arbitrary p, Arbitrary v)
    => Gen (Action k p v)
arbitraryAction = frequency
    [ (10, Insert <$> arbitrary <*> arbitrary <*> arbitrary)
    , (2,  pure DeleteRandomMember)
    , (2,  pure DeleteMin)
    ]

apply :: (PSQ psq, Ord p) => Action (Key psq) p v -> psq p v -> Gen (psq p v)
apply (Insert k p x)     t = return $ insert k p x t
apply DeleteRandomMember t = do
    key <- elements (keys t)
    return $ delete key t
apply DeleteMin          t = return $ case minView t of
    Nothing            -> t
    Just (_, _, _, t') -> t'

arbitraryPSQ
    :: forall psq p v. (Arbitrary (Key psq), Arbitrary p, Arbitrary v,
                        Ord p, PSQ psq)
    => Gen (psq p v)
arbitraryPSQ = do
    numActions <- choose (0, 100)
    actions    <- replicateM numActions arbitraryAction
    foldM (\t a -> apply a t) (empty :: psq p v) actions

shrinkPSQ
    :: forall psq p v. (Arbitrary (Key psq), Arbitrary p, Arbitrary v,
                        Ord p, PSQ psq)
    => psq p v -> [psq p v]
shrinkPSQ t = [delete k t | k <- keys t]

instance forall k p v. (Arbitrary k, Arbitrary p, Arbitrary v, Ord k, Ord p) =>
            Arbitrary (PSQ.PSQ k p v) where
    arbitrary = arbitraryPSQ
    shrink    = shrinkPSQ

instance forall p v. (Arbitrary p, Arbitrary v, Ord p) =>
            Arbitrary (IntPSQ.IntPSQ p v) where
    arbitrary = arbitraryPSQ
    shrink    = shrinkPSQ

instance forall k p v. (Arbitrary k, Arbitrary p, Arbitrary v,
                        Hashable k, Ord k, Ord p) =>
            Arbitrary (HashPSQ.HashPSQ k p v) where
    arbitrary = arbitraryPSQ
    shrink    = shrinkPSQ
