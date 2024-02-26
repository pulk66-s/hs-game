module Player (
    Player(..),
    newPlayer,
    findWeaponByName,
    holdWeapon,
    playerDamage,
    playerIsDead,
    printPlayer,
    hasKey,
    strengthStat
) where

import Item
import Item.Weapon
import Item.Key
import Item.Consumable
import List
import Player.Inventory
import Player.Data
import Player.Statistics

newPlayer :: Player
newPlayer   = Player 20 20 "Hugo" Nothing (List []) defaultPlayerStatistic 

holdWeapon :: Player -> Weapon -> Player
holdWeapon p w  = p { playerWeapon = Just w }

extractWeaponName :: Weapon -> String
extractWeaponName (WSword n)    = swordName n

findWeaponByName :: Player -> String -> Maybe Weapon
findWeaponByName p n    = unwrapItem (findInList findWeapon (inventory p))
    where
        findWeapon (IWeapon x)          = n == extractWeaponName x
        findWeapon _                    = False
        unwrapItem (Just (IWeapon w))   = Just w
        unwrapItem _                    = Nothing

playerDamage :: Player -> Int
playerDamage p  = getDmg (playerWeapon p)
    where
        getDmg Nothing  = 1
        getDmg (Just w) = weaponDamage w

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

hasKey :: Player -> Key -> Bool
hasKey p (Key name) = case findInList checkGoodKey (inventory p) of
    Just _  -> True
    Nothing -> False
    where
        checkGoodKey (IKey (Key n)) = n == name
        checkGoodKey _              = False
