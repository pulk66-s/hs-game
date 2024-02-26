module Item.Weapon (
    Weapon(..),
    Sword(..),
    excalibur,
    rustySword,
    weaponDamage,
    printWeapon
) where

data Sword = Sword {
    swordName :: String,
    damage :: Int
} deriving Show

newtype Weapon = WSword Sword
    deriving Show

excalibur :: Weapon
excalibur = WSword (Sword "Excalibur" 10)

rustySword :: Weapon
rustySword = WSword (Sword "Rusty Sword" 5)

weaponDamage :: Weapon -> Int
weaponDamage (WSword s) = damage s

printWeapon :: Weapon -> IO()
printWeapon (WSword s)  = putStr (swordName s ++ " with " ++ show (damage s) ++ " damage")
