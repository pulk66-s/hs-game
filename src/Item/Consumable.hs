module Item.Consumable (
    Consumable(..),
    HealthPotion(..),
    printConsumable,
    littleHealthPotion
) where

data HealthPotion = HealthPotion {
    healthPotionName :: String,
    healthPotionHealAmount :: Int
} deriving Show

newtype Consumable = CHealth HealthPotion
    deriving Show

littleHealthPotion :: Consumable
littleHealthPotion = CHealth (HealthPotion "Little_Health_Potion" 5)

printConsumable :: Consumable -> IO()
printConsumable (CHealth h) = putStr ("Health potion with " ++ show h ++ " health")
