-- | Higher-quality random generator for PSQ structures which generates a PSQ
-- from a series of actions
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
module Data.PSQ.Class.Gen
    ( arbitraryPSQ
    ) where

import           Control.Applicative (pure, (<$>), (<*>))
import           Test.QuickCheck     (Gen, Arbitrary (..), frequency, choose,
                                      elements)
import           Control.Monad       (foldM, replicateM)

import           Data.PSQ.Class      (PSQ (..))

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
