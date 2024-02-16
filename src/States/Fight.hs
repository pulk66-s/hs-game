module States.Fight (
    checkFight
) where

import Game
import Terminal
import List
import Enemies
import Room
import Player
import System.Exit

enemiesAttack :: Game -> List Enemy -> (Game -> IO()) -> IO()
enemiesAttack g (List []) next      = checkPlayerAlive g next
enemiesAttack g (List (x:xs)) next  = printAttack >> enemiesAttack updatedGame (List xs) next
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
    | isEnemies (room g)    = enemiesAttack g (enemies (room g)) next
    | otherwise             = next g

validateAttackAction :: Game -> Maybe Enemy -> (Game -> IO()) -> IO()
validateAttackAction g Nothing next     = putStrLn "Enemy not found" >> fightLoop g next
validateAttackAction g (Just e) next    = putStrLn str >> checkEnemiesAttack updatedGame next
    where
        str         = "You are attacking " ++ enemyName e
        updatedGame = g { room = (room g) { enemies = newEnemies } }
        newEnemies  = deleteDeadEnemies (updateEnemies (enemies (room g)) (attackEnemy (player g) e))

applyAction :: Game -> (Game -> IO()) -> Maybe FightCommand -> IO()
applyAction g n (Just (Attack name))
    | isEnnemyWithName (room g) name    = validateAttackAction g (findEnemyWithName (room g) name) n
    | otherwise                         = putStrLn "Enemy not found" >> fightLoop g n
applyAction g n (Just EnemyInfo)        = putStrLn "Here are the Enemies" >> listEnemies (enemies (room g)) >> fightLoop g n
applyAction g n Nothing                 = putStrLn "Command not found" >> fightLoop g n

fightLoop :: Game -> (Game -> IO()) -> IO()
fightLoop g n
    | isEnemies (room g)    = getFightCommand (applyAction g n)
    | otherwise             = putStrLn "You killed all enemies " >> n g

printStartFight :: Game -> IO()
printStartFight g   = putStrLn ("You are fighting " ++ show (lengthList e) ++ " Enemies") >> listEnemies e
    where
        e = enemies (room g)

launchFight :: Game -> (Game -> IO()) -> IO()
launchFight g n = printStartFight g >> fightLoop g n

checkFight :: Game -> (Game -> IO()) -> IO()
checkFight g next
    | isEnemies (room g)    = launchFight g next
    | otherwise             = next g
