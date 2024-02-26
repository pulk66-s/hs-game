module States.Fight.PlayerAttack (
    launchPlayerAttack
) where

import Game
import List
import Enemies
import Room
import States.Fight.EnemyAttack
import Dice
import Player.Statistics

applyAttack :: Game -> String -> IO Game
applyAttack game name   = case findInList nameFilter (enemies (getRoom game)) of
    Just enemy  -> do
        let damagedEnemy    = attackEnemy (player game) enemy
        if enemyIsDead damagedEnemy then putStrLn ("You killed " ++ name)
        else putStrLn ("You attacked " ++ name)
        let updatedEnemies  = updateEnemies (enemies (getRoom game)) damagedEnemy
        let aliveEnemies    = deleteDeadEnemies updatedEnemies
        let (index, r)      = room game
        return game { room = (index, r { enemies = aliveEnemies }) }
    Nothing     -> do
        putStrLn "Enemy not found"
        return game
    where
        nameFilter e    = enemyName e == name

launchAttackSuccess :: Game -> String -> IO Game
launchAttackSuccess game name  = do
    game'   <- applyAttack game name
    case enemies (getRoom game') of
        List [] -> return game'
        List e  -> do
            putStrLn "Enemies are attacking you"
            enemiesAttack game' e

launchAttackFailure :: Game -> IO Game
launchAttackFailure game    = do
    putStrLn "You missed your attack"
    case enemies (getRoom game) of
        List [] -> return game
        List e  -> do
            putStrLn "Enemies are attacking you"
            enemiesAttack game e

launchPlayerAttack :: Game -> String -> IO Game
launchPlayerAttack game name  = do
    diceRoll    <- randomDice 20
    putStrLn ("You rolled a " ++ show diceRoll)
    if diceRoll >= strengthStat (player game)
        then launchAttackSuccess game name
        else launchAttackFailure game
