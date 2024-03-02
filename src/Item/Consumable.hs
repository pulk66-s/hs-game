module Item.Consumable (
    Consumable(..),
    HealthPotion(..),
    printConsumable,
    printHealthPotion,
    littleHealthPotion,
    consumableName
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
printConsumable (CHealth h) = do
    putStr "Health potion: "
    printHealthPotion h

consumableName :: Consumable -> String
consumableName (CHealth h) = healthPotionName h

instance Eq HealthPotion where
    (==) (HealthPotion n1 h1) (HealthPotion n2 h2) = n1 == n2 && h1 == h2

instance Eq Consumable where
    (==) (CHealth h1) (CHealth h2) = h1 == h2
