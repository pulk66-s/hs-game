module Lib (
    play
) where

import Game
import Terminal
import Player
import Room
import Item

understandSearchCommand :: Game -> IO()
understandSearchCommand g   | null (loot (room g))  = print "You find nothing" >> gameLoop g
                            | otherwise             = print ("You find " ++ show (length items) ++ " items")
    >> printItemList items >> gameLoop (searchRoom g)
    where
        items   = loot (room g)

tryHoldingWeapon :: Game -> Maybe Weapon -> (Game -> IO()) -> IO()
tryHoldingWeapon g Nothing n    = print "You don't have that weapon" >> n g
tryHoldingWeapon g (Just w) n   = print ("You are now holding " ++ show w) >> n (g { player = holdWeapon (player g) w})

understandCommand :: Game -> Command -> IO()
understandCommand _ Exit            = print "Goodbye"
understandCommand g ShowMap         = print (show (room g)) >> gameLoop g
understandCommand g ShowHelp        = print "Commands: move [N|S|E|W], map, help, exit" >> gameLoop g
understandCommand g Search          = understandSearchCommand g
understandCommand g Inventory       = printInventory (player g)  >> gameLoop g
understandCommand g (HoldWeapon n)  = tryHoldingWeapon g (findWeaponByName (player g) n) gameLoop
understandCommand g PlayerData      = print (show (player g)) >> gameLoop g
understandCommand g (Move d)        = extractMaybe (moveRoom g d)
    where
        extractMaybe (Just g')  = print ("Moving to " ++ show d) >> gameLoop g'
        extractMaybe Nothing    = print "You can't go that way" >> gameLoop g

evaluateCommand :: Game -> Maybe Command -> IO()
evaluateCommand g Nothing   = print "Command not found" >> gameLoop g
evaluateCommand g (Just c)  = understandCommand g c

play :: IO()
play    = gameLoop (Game newPlayer newRoom)
    where
        newRoom     = testRooms

gameLoop :: Game -> IO()
gameLoop g  = getLine >>= evaluateCommand g . parseCommand
