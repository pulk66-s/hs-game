module Terminal (
    Command(..),
    FightCommand(..),
    parseCommand,
    getFightCommand,
    printPrompt
) where

import Room
import List
import System.IO

data Command =
    Move Direction      |
    Exit                |
    ShowMap             |
    ShowHelp            |
    Search              |
    ShowInventory       |
    HoldWeapon String   |
    PlayerData          |
    UseItem String
    

data FightCommand =
    Attack String       |
    EnemyInfo           |
    PlayerInfo          |
    UseItemFight String
    

moveCommand :: String -> Maybe Command
moveCommand "N"     = Just (Move North)
moveCommand "North" = Just (Move North)
moveCommand "S"     = Just (Move South)
moveCommand "South" = Just (Move South)
moveCommand "E"     = Just (Move East)
moveCommand "East"  = Just (Move East)
moveCommand "W"     = Just (Move West)
moveCommand "West"  = Just (Move West)
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
parseFightCommand "player"      = Just PlayerInfo
parseFightCommand cmd
    | startsWith "use " cmd     = Just (UseItemFight (drop 4 cmd))
parseFightCommand _             = Nothing

getFightCommand ::  IO (Maybe FightCommand)
getFightCommand = do
    printPrompt
    parseFightCommand <$> getLine

printPrompt :: IO()
printPrompt = putStrLn "===" >> putStrLn "Enter your command" >>
    putStr ">> " >> hFlush stdout
