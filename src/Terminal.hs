module Terminal (
    Command(..),
    FightCommand(..),
    parseCommand,
    getFightCommand
) where

import Room
import MyList

data Command =
    Move Direction      |
    Exit                |
    ShowMap             |
    ShowHelp            |
    Search              |
    ShowInventory       |
    HoldWeapon String   |
    PlayerData          
    deriving Show

data FightCommand =
    Attack String   |
    EnemyInfo
    deriving Show

moveCommand :: String -> Maybe Command
moveCommand "N"     = Just (Move North)
moveCommand "S"     = Just (Move South)
moveCommand "E"     = Just (Move East)
moveCommand "W"     = Just (Move West)
moveCommand _       = Nothing

parseCommand :: String -> Maybe Command
parseCommand "exit"             = Just Exit
parseCommand "help"             = Just ShowHelp
parseCommand "map"              = Just ShowMap
parseCommand "search"           = Just Search
parseCommand "inventory"        = Just ShowInventory
parseCommand "player"           = Just PlayerData
parseCommand cmd
    | startsWith "move " cmd    = moveCommand (drop 5 cmd)
    | startsWith "hold " cmd    = Just (HoldWeapon (drop 5 cmd))
parseCommand _                                  = Nothing

parseFightCommand :: String -> Maybe FightCommand
parseFightCommand cmd
    | startsWith "attack" cmd  = Just (Attack (drop 7 cmd))
parseFightCommand "enemies"     = Just EnemyInfo
parseFightCommand _             = Nothing

getFightCommand :: (Maybe FightCommand -> IO()) -> IO()
getFightCommand next    = getLine >>= next . parseFightCommand
