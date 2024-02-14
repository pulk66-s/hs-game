module States.Fight (
    checkFight
) where

import Game
import Terminal
import Enemies
import Room
import Player
import System.Exit

enemiesAttack :: Game -> [Enemy] -> (Game -> IO()) -> IO()
enemiesAttack g [] next     = checkPlayerAlive g next
enemiesAttack g (x:xs) next = printAttack >> enemiesAttack updatedGame xs next
    where
        updatedGame = g { player = enemyAttackPlayer x (player g) }
        printAttack = putStrLn ("You are attacked by " ++ enemyName x 
            ++ " that deals " ++ show (enemyDamage x) ++ " damages")

checkPlayerAlive :: Game -> (Game -> IO()) -> IO()
checkPlayerAlive g next
    | playerIsDead (player g)   = putStrLn "You are dead" >> exitSuccess
    | otherwise                 = fightLoop g next

checkEnemiesAttack :: Game -> (Game -> IO()) -> IO()
checkEnemiesAttack g next
    | isEnemies g   = enemiesAttack g (enemies (room g)) next
    | otherwise     = next g

validateAttackAction :: Game -> Maybe Enemy -> (Game -> IO()) -> IO()
validateAttackAction g Nothing next     = putStrLn "Enemy not found" >> fightLoop g next
validateAttackAction g (Just e) next    = putStrLn str >> checkEnemiesAttack updatedGame next
    where
        str         = "You are attacking " ++ enemyName e
        updatedGame = g { room = (room g) { enemies = newEnemies } }
        newEnemies  = deleteDeadEnemies (updateEnemies (enemies (room g)) (attackEnemy (player g) e))

applyAction :: Game -> (Game -> IO()) -> Maybe FightCommand -> IO()
applyAction g n (Just (Attack name))
    | enemyInRoom (room g) name     = validateAttackAction g (findEnemy (room g) name) n
    | otherwise                     = putStrLn "Enemy not found" >> fightLoop g n
applyAction g n (Just EnemyInfo)    = putStrLn "Here are the Enemies" >> listEnemies (enemies (room g)) >> fightLoop g n
applyAction g n Nothing             = putStrLn "Command not found" >> fightLoop g n

fightLoop :: Game -> (Game -> IO()) -> IO()
fightLoop g n
    | isEnemies g  = getFightCommand (applyAction g n)
    | otherwise     = putStrLn "You killed all enemies " >> n g

printStartFight :: Game -> IO()
printStartFight g   = putStrLn ("You are fighting " ++ show (length e) ++ " Enemies") >> listEnemies e
    where
        e = enemies (room g)

launchFight :: Game -> (Game -> IO()) -> IO()
launchFight g n = printStartFight g >> fightLoop g n

checkFight :: Game -> (Game -> IO()) -> IO()
checkFight g next   | isEnemies g  = launchFight g next
                    | otherwise     = next g
