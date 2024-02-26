module Player.Inventory (
    printPlayerInventory,
    addItemsToPlayerInventory,
    playerUseItemInFight
) where

import List
import Item
import Player.Data
import Item.Consumable

printPlayerInventory :: Player -> IO()
printPlayerInventory p = printList (inventory p) (\x -> printItem x >> putStrLn "")

addItemsToPlayerInventory :: Player -> [Item] -> Player
addItemsToPlayerInventory p items    = p {
    inventory = addList (inventory p) (List items)
}

removeItemFromPlayerInventory :: Player -> Item -> Player
removeItemFromPlayerInventory p item = p {
    inventory = removeList item (inventory p)
}

useHealthPotion :: Player -> HealthPotion -> Player
useHealthPotion player potion = player {
    playerHealth = min newHealth maxHealth
}
    where
        maxHealth   = playerMaxHealth player
        newHealth   = playerHealth player + healthPotionHealAmount potion

useConsumable :: Player -> Consumable -> Player
-- useConsumable player (CHealth p) = player {
--     playerHealth = playerHealth player + healthPotionHealAmount p
-- }
useConsumable player (CHealth p) = useHealthPotion player p

playerUseItemInFight :: Player -> Item -> Maybe Player
playerUseItemInFight player (IConsumable consumable) 
    = Just (removeItemFromPlayerInventory (useConsumable player consumable) (IConsumable consumable))
playerUseItemInFight _ _
    = Nothing
