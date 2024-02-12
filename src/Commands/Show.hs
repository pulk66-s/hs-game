module Commands.Show (
    showCommands
) where

import Game
import Terminal
import Player

showMap :: Game -> IO()
showMap g   = print (show (room g))

showHelp :: IO()
showHelp   = print "Commands: move [N|S|E|W], map, help, exit"

showInventory :: Game -> IO()
showInventory g = printInventory (player g)

showPlayerData :: Game -> IO()
showPlayerData g = print (show (player g))

showCommands :: Game -> Command -> (Game -> IO()) -> IO()
showCommands g ShowMap n        = showMap g >> n g
showCommands g ShowHelp n       = showHelp >> n g
showCommands g ShowInventory n  = showInventory g >> n g
showCommands g PlayerData n     = showPlayerData g >> n g
showCommands _ _ _              = return ()
