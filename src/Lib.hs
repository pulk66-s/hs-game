module Lib (
    play
) where

import Game
import Terminal
import Player
import Item
import Commands.Show
import Commands.Move
import Commands.Search
import Map.Default

tryHoldingWeapon :: Game -> Maybe Weapon -> (Game -> IO()) -> IO()
tryHoldingWeapon g Nothing n    = print "You don't have that weapon" >> n g
tryHoldingWeapon g (Just w) n   = print ("You are now holding " ++ show w) >> n (g { player = holdWeapon (player g) w})

understandCommand :: Game -> Command -> IO()
understandCommand _ Exit            = print "Goodbye"
understandCommand g (HoldWeapon n)  = tryHoldingWeapon g (findWeaponByName (player g) n) gameLoop
understandCommand g (Move d)        = moveCommand g d gameLoop
understandCommand g Search          = searchCommand g gameLoop
understandCommand g cmd             = showCommands g cmd gameLoop

evaluateCommand :: Game -> Maybe Command -> IO()
evaluateCommand g Nothing   = print "Command not found" >> gameLoop g
evaluateCommand g (Just c)  = understandCommand g c

play :: IO()
play    = gameLoop (Game newPlayer newRoom)
    where
        newRoom     = defaultRoom

gameLoop :: Game -> IO()
gameLoop g  = getLine >>= evaluateCommand g . parseCommand
