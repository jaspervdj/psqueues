-- | Higher-quality random generator for PSQ structures which generates a PSQ
-- from a series of actions
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
module Data.PSQ.Class.Gen
    ( arbitraryPSQ
    ) where

import           Control.Applicative (pure, (<$>), (<*>))
import           Test.QuickCheck     (Gen, Arbitrary (..), frequency, choose)
import           Data.List           (foldl')
import           Control.Monad       (replicateM)

import           Data.PSQ.Class      (PSQ (..))

data Action k p v
    = Insert k p v
    | Delete k
    | DeleteMin
    deriving (Show, Eq)

arbitraryAction
    :: (Arbitrary k, Arbitrary p, Arbitrary v)
    => Gen (Action k p v)
arbitraryAction = frequency
    [ (10, Insert <$> arbitrary <*> arbitrary <*> arbitrary)
    , (1,  Delete <$> arbitrary)
    , (2,  pure DeleteMin)
    ]

apply :: (PSQ psq, Ord p) => Action (Key psq) p v -> psq p v -> psq p v
apply (Insert k p x) t = insert k p x t
apply (Delete k)     t = delete k t
apply DeleteMin      t = case minView t of
    Nothing            -> t
    Just (_, _, _, t') -> t'

arbitraryPSQ
    :: forall psq p v. (Arbitrary (Key psq), Arbitrary p, Arbitrary v,
                        Ord p, PSQ psq)
    => Gen (psq p v)
arbitraryPSQ = do
    numActions <- choose (0, 100)
    actions    <- replicateM numActions arbitraryAction
    return $ foldl' (\a t -> apply t a) (empty :: psq p v) actions
