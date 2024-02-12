module Terminal (
    Command(..),
    parseCommand
) where

import Room
import MyList

data Command =
    Move Direction  |
    Exit            |
    ShowMap         |
    ShowHelp        |
    Search          |
    Inventory
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
parseCommand cmd    | startsWith "move " cmd    = moveCommand (drop 5 cmd)
                    | otherwise                 = Nothing
