module Item (
    Item(..),
    Weapon(..),
    Sword(..),
    Key(..),
    Consumable(..),
    HealthPotion(..),
    excalibur,
    rustySword,
    weaponDamage,
    printWeapon,
    printItems,
    printItem,
    littleHealthPotion
) where

data Sword = Sword {
    swordName :: String,
    damage :: Int
} deriving Show

data HealthPotion = HealthPotion {
    healthPotionName :: String,
    healthPotionHealAmount :: Int
} deriving Show

newtype Weapon = WSword Sword
    deriving Show

newtype Consumable = CHealth HealthPotion
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

littleHealthPotion :: Consumable
littleHealthPotion = CHealth (HealthPotion "Little_Health_Potion" 5)

weaponDamage :: Weapon -> Int
weaponDamage (WSword s) = damage s

printWeapon :: Weapon -> IO()
printWeapon (WSword s)  = putStr (swordName s ++ " with " ++ show (damage s) ++ " damage")

printKey :: Key -> IO()
printKey (Key k) = putStr k

printConsumable :: Consumable -> IO()
printConsumable (CHealth h) = putStr ("Health potion with " ++ show h ++ " health")

printItem :: Item -> IO()
printItem (IWeapon w)       = printWeapon w
printItem (IKey k)          = printKey k
printItem (IConsumable c)   = printConsumable c

printItems :: [Item] -> IO()
printItems = foldr (\x -> (>>) (printItem x >> putStrLn "")) (putStrLn "")
