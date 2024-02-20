module Item (
    Item(..),
    Weapon(..),
    Sword(..),
    Key(..),
    excalibur,
    rustySword,
    weaponDamage,
    printWeapon,
    printItems,
    printItem
) where

data Sword = Sword {
    swordName :: String,
    damage :: Int
} deriving Show

newtype Weapon = WSword Sword
    deriving Show

newtype Consumable = CHealth Int
    deriving Show

newtype Key = Key String
    deriving Show

data Item =
    IWeapon Weapon          |
    IKey Key                |
    IConsumable Consumable
    deriving Show

excalibur :: Weapon
excalibur = WSword (Sword "Excalibur" 10)

rustySword :: Weapon
rustySword = WSword (Sword "Rusty Sword" 5)

weaponDamage :: Weapon -> Int
weaponDamage (WSword s) = damage s

printWeapon :: Weapon -> IO()
printWeapon (WSword s)  = putStr (swordName s ++ " with " ++ show (damage s) ++ " damage")

printKey :: Key -> IO()
printKey (Key k) = putStrLn k

printConsumable :: Consumable -> IO()
printConsumable (CHealth h) = putStrLn ("Health potion with " ++ show h ++ " health")

printItem :: Item -> IO()
printItem (IWeapon w)       = printWeapon w
printItem (IKey k)          = printKey k
printItem (IConsumable c)   = printConsumable c

printItems :: [Item] -> IO()
printItems = foldr (\x -> (>>) (printItem x >> putStrLn "")) (putStrLn "")
