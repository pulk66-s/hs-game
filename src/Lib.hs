module Lib (
    play
) where

import Game
import Terminal
import Player
import Item
import Item.Weapon
import Commands.Show
import Commands.Move
import Commands.Search

tryHoldingWeapon :: Game -> Maybe Weapon -> (Game -> IO()) -> IO()
tryHoldingWeapon g Nothing n    = putStrLn "You don't have that weapon" >> n g
-- tryHoldingWeapon g (Just w) n   = putStrLn ("You are now holding " ++ show w) >> n (g { player = holdWeapon (player g) w})
tryHoldingWeapon g (Just w) n   = do
    putStr "You are now holding: "
    printWeapon w
    n (g { player = holdWeapon (player g) w})

understandCommand :: Game -> Command -> IO()
understandCommand _ Exit            = putStrLn "Goodbye"
understandCommand g (HoldWeapon n)  = tryHoldingWeapon g (findWeaponByName (player g) n) gameLoop
understandCommand g (Move d)        = moveCommand g d gameLoop
understandCommand g Search          = searchCommand g gameLoop
understandCommand g cmd             = showCommands g cmd gameLoop

evaluateCommand :: Game -> Maybe Command -> IO()
evaluateCommand g Nothing   = putStrLn "Command not found" >> gameLoop g
evaluateCommand g (Just c)  = understandCommand g c

play :: IO()
play    = gameLoop defaultGame

gameLoop :: Game -> IO()
gameLoop g  = printPrompt >> getLine >>= evaluateCommand g . parseCommand
