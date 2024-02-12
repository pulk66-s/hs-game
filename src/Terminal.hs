module Terminal (
    Command(..),
    parseCommand
) where

import Room
import MyList

data Command =
    Move Direction      |
    Exit                |
    ShowMap             |
    ShowHelp            |
    Search              |
    Inventory           |
    HoldWeapon String   |
    PlayerData          
    deriving Show

moveCommand :: String -> Maybe Command
moveCommand "N"     = Just (Move North)
moveCommand "S"     = Just (Move South)
moveCommand "E"     = Just (Move East)
moveCommand "W"     = Just (Move West)
moveCommand _       = Nothing

parseCommand :: String -> Maybe Command
parseCommand "exit"                             = Just Exit
parseCommand "help"                             = Just ShowHelp
parseCommand "map"                              = Just ShowMap
parseCommand "search"                           = Just Search
parseCommand "inventory"                        = Just Inventory
parseCommand "player"                           = Just PlayerData
parseCommand cmd    | startsWith "move " cmd    = moveCommand (drop 5 cmd)
                    | startsWith "hold " cmd    = Just (HoldWeapon (drop 5 cmd))
parseCommand _                                  = Nothing
