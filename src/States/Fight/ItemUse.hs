module States.Fight.ItemUse (
    useItemInFight
) where

import Player.Inventory
import Player.Data
import Game
import Item
import List

checkItemUsage :: Game -> Item -> Maybe Player -> IO Game
checkItemUsage game item (Just p)   = do
    putStrLn ("You used " ++ itemName item ++ " item")
    return game { player = p }
checkItemUsage game _ Nothing       = do
    putStrLn "You can't use this item"
    return game

itemExists :: Game -> Item -> IO Game
itemExists game item    = checkItemUsage game item (playerUseItemInFight (player game) item)

itemDoesntExists :: Game -> IO Game
itemDoesntExists game   = do
    putStrLn "Item not found"
    return game

isItemInInventory :: Game -> Maybe Item -> IO Game
isItemInInventory game (Just item)  = itemExists game item
isItemInInventory game Nothing      = itemDoesntExists game

useItemInFight :: Game -> String -> IO Game
useItemInFight game name    = isItemInInventory game (
        findInList (itemNameFilter name) playerInventory
    )
    where
        playerInventory = inventory (player game)
