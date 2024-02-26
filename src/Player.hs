module Player (
    Player(..),
    newPlayer,
    printPlayerInventory,
    findWeaponByName,
    holdWeapon,
    playerDamage,
    playerIsDead,
    printPlayer,
    addItems,
    hasKey,
    strengthStat
) where

import Item
import List

data PlayerStatistic = PlayerStatistic {
    playerStrength :: Int,
    playerDexterity :: Int,
    playerIntelligence :: Int
} deriving Show

data Player = Player {
    playerHealth :: Int,
    playerName :: String,
    playerWeapon :: Maybe Weapon,
    inventory :: List Item,
    playerStatistic :: PlayerStatistic
} deriving Show

defaultPlayerStatistic :: PlayerStatistic
defaultPlayerStatistic  = PlayerStatistic 10 10 10

newPlayer :: Player
newPlayer   = Player 20 "Hugo" Nothing (List []) defaultPlayerStatistic

printPlayerInventory :: Player -> IO()
printPlayerInventory p = printList (inventory p) (\x -> printItem x >> putStrLn "")

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

addItems :: Player -> [Item] -> Player
addItems p items    = p { inventory = addList (inventory p) (List items) }

hasKey :: Player -> Key -> Bool
hasKey p (Key name) = case findInList checkGoodKey (inventory p) of
    Just _  -> True
    Nothing -> False
    where
        checkGoodKey (IKey (Key n)) = n == name
        checkGoodKey _              = False

strengthStat :: Player -> Int
strengthStat p  = playerStrength (playerStatistic p)
