module Player (
    Player(..),
    newPlayer,
    printInventory
) where

import Item

data Player = Player {
    name :: String,
    weapon :: Maybe Weapon,
    inventory :: [Item]
} deriving Show

newPlayer :: Player
newPlayer   = Player "Hugo" Nothing []

printInventory :: Player -> IO()
printInventory p    = print (show (inventory p))
