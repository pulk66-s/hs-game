module Player (
    Player(..),
    newPlayer,
    printPlayerInventory,
    findWeaponByName,
    holdWeapon,
    playerDamage,
    playerIsDead,
    printPlayer,
    addItems
) where

import Item
import List

data Player = Player {
    playerHealth :: Int,
    playerName :: String,
    playerWeapon :: Maybe Weapon,
    inventory :: List Item
} deriving Show

newPlayer :: Player
newPlayer   = Player 20 "Hugo" Nothing (List [])

printPlayerInventory :: Player -> IO()
printPlayerInventory p = printList (inventory p) printItem

holdWeapon :: Player -> Weapon -> Player
holdWeapon p w  = p { playerWeapon = Just w }

extractWeaponName :: Weapon -> String
extractWeaponName (WSword n)    = swordName n

findWeaponByName :: Player -> String -> Maybe Weapon
findWeaponByName p n    = unwrapItem (findInList findWeapon (inventory p))
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
printPlayer p   = do
    putStrLn "Here is your player datas"
    putStrLn ("Name: " ++ playerName p)
    putStrLn ("Health: " ++ show (playerHealth p))
    putStrLn "Inventory: "
    printPlayerInventory p
    putStrLn "\nWeapon: "
    printPlayerWeapon (playerWeapon p)

addItems :: Player -> [Item] -> Player
addItems p items    = p { inventory = addList (inventory p) (List items) }
