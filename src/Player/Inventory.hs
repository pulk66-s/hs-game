module Player.Inventory (
    printPlayerInventory,
    addItemsToPlayerInventory
) where

import List
import Item
import Player.Data

printPlayerInventory :: Player -> IO()
printPlayerInventory p = printList (inventory p) (\x -> printItem x >> putStrLn "")

addItemsToPlayerInventory :: Player -> [Item] -> Player
addItemsToPlayerInventory p items    = p {
    inventory = addList (inventory p) (List items)
}
