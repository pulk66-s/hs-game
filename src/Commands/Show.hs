module Commands.Show (
    showCommands
) where

import Game
import Terminal
import Player
import Room
import List
import Item 

showMap :: Game -> IO()
showMap g   = printRoom (room g)

showHelp :: IO()
showHelp   = putStrLn "Commands: move [N|S|E|W], map, help, exit"

showInventory :: Game -> IO()
showInventory g = printList (inventory (player g)) printItem

showCommands :: Game -> Command -> (Game -> IO()) -> IO()
showCommands g ShowMap n        = showMap g >> n g
showCommands g ShowHelp n       = showHelp >> n g
showCommands g ShowInventory n  = showInventory g >> n g
showCommands g PlayerData n     = printPlayer (player g) >> putStrLn "" >> n g
showCommands _ _ _              = return ()
