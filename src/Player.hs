module Player (
    Player(..),
    newPlayer,
    printPlayetrInventory,
    findWeaponByName,
    holdWeapon,
    playerDamage,
    playerIsDead,
    printPlayer,
    addItems
) where

import Item
import Player.Inventory

data Player = Player {
    playerHealth :: Int,
    playerName :: String,
    playerWeapon :: Maybe Weapon,
    inventory :: Inventory Item
} deriving Show

newPlayer :: Player
newPlayer   = Player 20 "Hugo" (Just excalibur) (Inventory [])

printPlayetrInventory :: Player -> IO()
printPlayetrInventory p = printInventory (inventory p)

holdWeapon :: Player -> Weapon -> Player
holdWeapon p w  = p { playerWeapon = Just w }

extractWeaponName :: Weapon -> String
extractWeaponName (WSword n)    = swordName n

findWeaponByName :: Player -> String -> Maybe Weapon
findWeaponByName p n    = unwrapItem (inventoryFind findWeapon (inventory p))
    where
        findWeapon (IWeapon x)          = n == extractWeaponName x
        unwrapItem (Just (IWeapon w))   = Just w
        unwrapItem _                    = Nothing

playerDamage :: Player -> Int
playerDamage (Player _ _ Nothing _)     = 1
playerDamage (Player _ _ (Just w) _)    = weaponDamage w

playerIsDead :: Player -> Bool
playerIsDead p  = playerHealth p <= 0

printPlayerWeapon :: Maybe Weapon -> IO()
printPlayerWeapon Nothing   = putStrLn "No weapon"
printPlayerWeapon (Just w)  = printWeapon w

printPlayer :: Player -> IO()
printPlayer p   = putStrLn "Here is your player datas"
    >> putStrLn ("Name: " ++ playerName p)
    >> putStrLn ("Health: " ++ show (playerHealth p))
    >> putStrLn "Inventory: " >> printPlayetrInventory p
    >> putStrLn "Weapon: " >> printPlayerWeapon (playerWeapon p)

addItems :: Player -> [Item] -> Player
addItems p items    = p { inventory = foldr addInventory (inventory p) items }
