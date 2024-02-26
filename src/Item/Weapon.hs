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
    swordDamage :: Int
} 

newtype Weapon = WSword Sword

excalibur :: Weapon
excalibur = WSword (Sword "Excalibur" 10)

rustySword :: Weapon
rustySword = WSword (Sword "Rusty Sword" 5)

weaponDamage :: Weapon -> Int
weaponDamage (WSword s) = swordDamage s

weaponName :: Weapon -> String
weaponName (WSword s) = swordName s

printWeapon :: Weapon -> IO()
printWeapon w   = putStr (weaponName w ++ " with " ++ show (weaponDamage w) ++ " damage")
