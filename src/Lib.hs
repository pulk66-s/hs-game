module Lib (
    play
) where

import Game
import Terminal
import Room
import Item
import Player
import Player.Inventory
import Item.Weapon
import Commands.Show
import Commands.Move
import Commands.Search

tryHoldingWeapon :: Game -> Maybe Weapon -> (Game -> IO()) -> IO()
tryHoldingWeapon g Nothing n    = putStrLn "You don't have that weapon" >> n g
tryHoldingWeapon g (Just w) n   = do
    putStr "You are now holding: "
    printWeapon w
    n (g { player = holdWeapon (player g) w})

evaluateGrabCommand :: Game -> String -> (Game -> IO()) -> IO()
evaluateGrabCommand g name next = case grabItemFromRoom name (getRoom g) of
    Nothing     -> putStrLn "You can't grab that" >> next g
    Just item   -> do
        putStrLn "You grabbed: "
        printItem item
        putStrLn ""
        let (i, r)  = room g
        let r'      = deleteItemFromRoom item r
        let p'      = addItemToPlayerInventory (player g) item
        next (g { player = p', room = (i, r') })

understandCommand :: Game -> Command -> (Game -> IO()) -> IO()
understandCommand _ Exit _              = putStrLn "Goodbye"
understandCommand g (HoldWeapon n) next = tryHoldingWeapon g (findWeaponByName (player g) n) next
understandCommand g (Move d) next       = moveCommand g d next
understandCommand g (Grab item) next    = evaluateGrabCommand g item next
understandCommand g Search next         = searchCommand g next
understandCommand g cmd next            = showCommands g cmd next

evaluateCommand :: Game -> Maybe Command -> (Game -> IO()) -> IO()
evaluateCommand g Nothing _     = do
    putStrLn "Command not found"
    gameLoop g
evaluateCommand g (Just c) next = understandCommand g c next

play :: IO()
play    = gameLoop defaultGame

gameLoop :: Game -> IO()
gameLoop g  = do
    if checkWinCondition g
        then do
            putStrLn "You won!"
            return ()
        else do
            printPrompt
            input <- getLine
            evaluateCommand g (parseCommand input) gameLoop
