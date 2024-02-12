module Item (
    Item(..),
    Weapon(..),
    Sword(..)
) where

data Sword = Sword {
    name :: String,
    damage :: Int
} deriving Show

newtype Weapon = Weapon Sword
    deriving Show

newtype Item = Item Weapon
    deriving Show
