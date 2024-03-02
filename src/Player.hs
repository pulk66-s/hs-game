module Player (
    Player(..),
    newPlayer,
    findWeaponByName,
    holdWeapon,
    playerIsDead,
    printPlayer,
    hasKey,
    strengthStat
) where

import Item
import Item.Weapon
import Item.Key
import List
import Player.Inventory
import Player.Data
import Player.Statistics
import Player.Magic

newPlayer :: Player
newPlayer   = Player 20 20 "Hugo" Nothing (List []) defaultPlayerStatistic defaultPlayerMagic

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

playerIsDead :: Player -> Bool
playerIsDead p  = playerHealth p <= 0

printPlayerWeapon :: Maybe Weapon -> IO()
printPlayerWeapon Nothing   = putStrLn "No weapon"
printPlayerWeapon (Just w)  = printWeapon w

printSpell :: PlayerSpell -> IO()
printSpell s    = do
    putStr ("Name: " ++ spellName s)
    putStr (", Damage: " ++ show (spellDamage s))
    putStrLn (", Mana Cost: " ++ show (spellManaCost s))

printSpellCategory :: (Int, List PlayerSpell) -> IO()
printSpellCategory (n, List l)   = do
    putStrLn ("Level " ++ show n)
    printList (List l) printSpell

printSpellList :: List (Int, List PlayerSpell) -> IO()
printSpellList inv  = do
    let l   = unwrapList inv
    mapM_ printSpellCategory l

printPlayerMagic :: PlayerMagic -> IO()
printPlayerMagic magic  = do
    putStrLn ("Mana " ++ show (playerMana magic))
    putStrLn "Spell List:"
    printSpellList (spellInventory magic)

printPlayer :: Player -> IO()
printPlayer p   = do
    putStrLn "Here is your player datas"
    putStrLn ("Name: " ++ playerName p)
    putStrLn ("Health: " ++ show (playerHealth p))
    putStrLn "Inventory: "
    printPlayerInventory p
    putStrLn "\nWeapon: "
    printPlayerWeapon (playerWeapon p)
    putStrLn "Magic: "
    printPlayerMagic (playerMagic p)

hasKey :: Player -> Key -> Bool
hasKey p (Key name) = case findInList checkGoodKey (inventory p) of
    Just _  -> True
    Nothing -> False
    where
        checkGoodKey (IKey (Key n)) = n == name
        checkGoodKey _              = False
