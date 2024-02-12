module Item (
    Item(..),
    Weapon(..),
    Sword(..),
    excalibur,
    rustySword
) where

data Sword = Sword {
    name :: String,
    damage :: Int
} deriving Show

newtype Weapon = Weapon Sword
    deriving Show

newtype Item = Item Weapon
    deriving Show

excalibur :: Weapon
excalibur = Weapon (Sword "Excalibur" 10)

rustySword :: Weapon
rustySword = Weapon (Sword "Rusty Sword" 5)
