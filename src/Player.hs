module Player (
    Player(..),
    newPlayer,
    printInventory,
    findWeaponByName,
    holdWeapon,
    playerDamage
) where

import Item

data Player = Player {
    name :: String,
    playerWeapon :: Maybe Weapon,
    inventory :: [Item]
} deriving Show

newPlayer :: Player
newPlayer   = Player "Hugo" (Just excalibur) []

printInventory :: Player -> IO()
printInventory p    = print (show (inventory p))

holdWeapon :: Player -> Weapon -> Player
holdWeapon p w  = p { playerWeapon = Just w }

extractWeaponName :: Weapon -> String
extractWeaponName (WSword n)    = swordName n

findWeaponByName :: Player -> String -> Maybe Weapon
findWeaponByName p n    = findWeapon n (inventory p)
    where
        findWeapon _ []    = Nothing
        findWeapon n (IWeapon x:xs) 
            | n == extractWeaponName x  = Just x
            | otherwise                 = findWeapon n xs

playerDamage :: Player -> Int
playerDamage (Player _ Nothing _)   = 1
playerDamage (Player _ (Just w) _)  = weaponDamage w
