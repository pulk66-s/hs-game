module Item.Consumable (
    Consumable(..),
    HealthPotion(..),
    printConsumable,
    printHealthPotion,
    littleHealthPotion
) where

data HealthPotion = HealthPotion {
    healthPotionName :: String,
    healthPotionHealAmount :: Int
} 

newtype Consumable = CHealth HealthPotion
    
littleHealthPotion :: Consumable
littleHealthPotion = CHealth (HealthPotion "Little_Health_Potion" 5)

printHealthPotion :: HealthPotion -> IO()
printHealthPotion potion    = do
    putStr (healthPotionName potion)
    putStr " with "
    putStr (show (healthPotionHealAmount potion))
    putStrLn " health"

printConsumable :: Consumable -> IO()
-- printConsumable (CHealth h) = putStr ("Health potion with " ++ show h ++ " health")
printConsumable (CHealth h) = do
    putStr "Health potion: "
    printHealthPotion h
