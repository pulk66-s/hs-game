module Item (
    Item(..),
    printItems,
    printItem
) where

import Item.Weapon
import Item.Key
import Item.Consumable

data Item =
    IWeapon Weapon          |
    IKey Key                |
    IConsumable Consumable
    

printItem :: Item -> IO()
printItem (IWeapon w)       = printWeapon w
printItem (IKey k)          = printKey k
printItem (IConsumable c)   = printConsumable c

printItems :: [Item] -> IO()
printItems = foldr (\x -> (>>) (printItem x >> putStrLn "")) (putStrLn "")
