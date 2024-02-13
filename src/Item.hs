module Item (
    Item(..),
    Weapon(..),
    Sword(..),
    excalibur,
    rustySword,
    weaponDamage
) where

data Sword = Sword {
    swordName :: String,
    damage :: Int
} deriving Show

newtype Weapon = WSword Sword
    deriving Show

newtype Item = IWeapon Weapon
    deriving Show

excalibur :: Weapon
excalibur = WSword (Sword "Excalibur" 10)

rustySword :: Weapon
rustySword = WSword (Sword "Rusty Sword" 5)

weaponDamage :: Weapon -> Int
weaponDamage (WSword s) = damage s
