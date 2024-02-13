module Enemies (
    Enemy(..),
    goblin,
    listEnemies,
    printEnemy,
    dealDamageToEnemy,
    updateEnemies,
    attackEnemy,
    deleteDeadEnemies
) where

import Item
import Player

data Enemy = Enemy {
    enemyName :: String,
    health :: Int,
    enemyWeapon :: Weapon
} deriving Show

goblin :: Enemy
goblin  = Enemy "Goblin" 15 rustySword

listEnemies :: [Enemy] -> IO()
listEnemies [] = print "There is no Enemy"
listEnemies e  = mapM_ printEnemy e

printEnemy :: Enemy -> IO()
printEnemy e   = print (enemyName e ++ " with " ++ show (health e) ++ " health and a " ++ show (enemyWeapon e))

dealDamageToEnemy :: Enemy -> Int -> Enemy
dealDamageToEnemy e amount = e { health = health e - amount }

isDead :: Enemy -> Bool
isDead e    = health e <= 0

attackEnemy :: Player -> Enemy -> Enemy
attackEnemy p e    = e { health = health e - playerAttack }
    where
        playerAttack    = playerDamage p

updateEnemies :: [Enemy] -> Enemy -> [Enemy]
updateEnemies [] _                  = []
updateEnemies (x:xs) n
    | enemyName x == enemyName n    = n : updateEnemies xs n
    | otherwise                     = x : updateEnemies xs n

deleteDeadEnemies :: [Enemy] -> [Enemy]
deleteDeadEnemies   = filter (not . isDead)
