module Item (
    Item(..),
    Weapon(..),
    Sword(..),
    excalibur,
    rustySword,
    weaponDamage,
    printWeapon,
    printItems
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
rustySword = WSword (Sword "Rusty Sword" 50)

weaponDamage :: Weapon -> Int
weaponDamage (WSword s) = damage s

printWeapon :: Weapon -> IO()
printWeapon (WSword s)  = putStr (swordName s ++ " with " ++ show (damage s) ++ " damage")

printItem :: Item -> IO()
printItem (IWeapon w)   = printWeapon w

printItems :: [Item] -> IO()
printItems = foldr (\x -> (>>) (printItem x >> putStrLn "")) (putStrLn "")
