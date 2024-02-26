module States.Fight (
    checkFight
) where

import Game
import Room
import Enemies
import Player
import Terminal
import Item
import Item.Weapon
import Item.Consumable
import Item.Key
import List
import Dice
import States.Fight.Command

fightLoop :: Game -> IO Game
fightLoop game
    | roomHasEnemies (getRoom game) = do
        putStrLn "\nWhat do you want to do ?"
        cmd     <- getFightCommand
        game'   <- evaluateFightCommand game cmd
        fightLoop game'
    | otherwise                     = do
        putStrLn "You killed all enemies"
        return game

launchFight :: Game -> (Game -> IO()) -> IO()
launchFight game next   = do
    putStrLn "You are attacked !"
    putStrLn "Here are the enemies:"
    listEnemies (enemies (getRoom game))
    game'   <- fightLoop game
    next game'

checkFight :: Game -> (Game -> IO()) -> IO()
checkFight game next
    | roomHasEnemies (getRoom game) = launchFight game next
    | otherwise                     = next game
