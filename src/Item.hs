
module Item (
    Item(..),
    printItems,
    printItem,
    itemNameFilter,
    itemName
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

itemName :: Item -> String
itemName (IWeapon weapon)   = weaponName weapon
itemName (IKey key)         = keyName key
itemName (IConsumable c)    = consumableName c

itemNameFilter :: String -> Item -> Bool
itemNameFilter name item    = itemName item == name

-- Not used but here is an implementation
_lookupItem :: Item -> [Item] -> Maybe String
_lookupItem item list   = case findInList (== item) list of
    Just _  -> Just (itemName item)
    _       -> Nothing

instance Eq Item where
    (==) (IWeapon w1) (IWeapon w2)          = w1 == w2
    (==) (IKey k1) (IKey k2)                = k1 == k2
    (==) (IConsumable c1) (IConsumable c2)  = c1 == c2
    (==) _ _                                = False
