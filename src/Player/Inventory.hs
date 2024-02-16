module Player.Inventory (
    Inventory(..),
    removeInventory,
    addInventory,
    printInventory,
    inventoryFind
) where

newtype Inventory a = Inventory [a] deriving Show

addInventory :: a -> Inventory a -> Inventory a
addInventory x (Inventory xs) = Inventory (x:xs)

removeInventory :: Eq a => a -> Inventory a -> Inventory a
removeInventory x (Inventory xs) = Inventory (filter (/= x) xs)

printInventory :: Show a => Inventory a -> IO()
printInventory (Inventory []) = putStrLn "There is no item in the inventory"
printInventory (Inventory xs) = mapM_ print xs

inventoryFind :: (a -> Bool) -> Inventory a -> Maybe a
inventoryFind f (Inventory xs) = unwrapFilter (filter f xs)
    where
        unwrapFilter []     = Nothing
        unwrapFilter (x:_)  = Just x
