module Player (
    Player(..),
    newPlayer,
    printInventory,
    findWeaponByName,
    holdWeapon
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

holdWeapon :: Player -> Weapon -> Player
holdWeapon p w  = p { weapon = Just w }

extractWeaponName :: Weapon -> String
extractWeaponName (Weapon (Sword n _)) = n

findWeaponByName :: Player -> String -> Maybe Weapon
findWeaponByName p n    = findWeapon n (inventory p)
    where
        findWeapon _ []    = Nothing
        findWeapon n (Item (Weapon x):xs)   | n == extractWeaponName (Weapon x) = Just (Weapon x)
                                            | otherwise                         = findWeapon n xs
