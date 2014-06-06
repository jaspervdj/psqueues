module Data.PSQ.Internal.Types
    (
    -- * Binding Type
    Elem(..)
    ) where

-- | @E k p@ binds the key @k@ with the priority @p@.
data Elem k p v = E
    { key   :: !k
    , prio  :: !p
    , value :: !v
    } deriving (Eq, Show)