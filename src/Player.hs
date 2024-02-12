module Player (
    Player(..)
) where

newtype Player = Player {
    name :: String
} deriving Show
