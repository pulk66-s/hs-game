module States.Fight (
    checkFight
) where

import Game
import Terminal
import Enemies
import Room

validateAttackAction :: Game -> Maybe Enemy -> (Game -> IO()) -> IO()
validateAttackAction g Nothing next     = print "Enemy not found" >> fightLoop g next
validateAttackAction g (Just e) next    = print str >> fightLoop updatedGame next
    where
        str         = "You are attacking " ++ enemyName e
        updatedGame = g { room = (room g) { enemies = newEnemies } }
        newEnemies  = deleteDeadEnemies (updateEnemies (enemies (room g)) (attackEnemy (player g) e))

applyAction :: Game -> (Game -> IO()) -> Maybe FightCommand -> IO()
applyAction g n (Just (Attack name))
    | enemyInRoom (room g) name     = validateAttackAction g (findEnemy (room g) name) n
    | otherwise                     = print "Enemy not found" >> fightLoop g n
applyAction g n (Just EnemyInfo)    = print "Here are the Enemies" >> listEnemies (enemies (room g)) >> fightLoop g n
applyAction g n Nothing             = print "Command not found" >> fightLoop g n

fightLoop :: Game -> (Game -> IO()) -> IO()
fightLoop g n
    | isEnemies g  = getFightCommand (applyAction g n)
    | otherwise     = print "You killed all enemies " >> n g

printStartFight :: Game -> IO()
printStartFight g   = print ("You are fighting " ++ show (length e) ++ " Enemies") >> listEnemies e
    where
        e = enemies (room g)

launchFight :: Game -> (Game -> IO()) -> IO()
launchFight g n = printStartFight g >> fightLoop g n

checkFight :: Game -> (Game -> IO()) -> IO()
checkFight g next   | isEnemies g  = launchFight g next
                    | otherwise     = next g
