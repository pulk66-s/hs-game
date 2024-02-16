module Enemies (
    Enemy(..),
    goblin,
    listEnemies,
    printEnemy,
    dealDamageToEnemy,
    updateEnemies,
    attackEnemy,
    deleteDeadEnemies,
    enemyAttackPlayer,
    enemyDamage
) where

import Item
import Player
import List

data Enemy = Enemy {
    enemyName :: String,
    enemyHealth :: Int,
    enemyWeapon :: Weapon
} deriving Show

goblin :: Enemy
goblin  = Enemy "Goblin" 15 rustySword

listEnemies :: List Enemy -> IO()
listEnemies (List [])   = putStrLn "There is no Enemy"
listEnemies e           = printList e printEnemy

printEnemy :: Enemy -> IO()
printEnemy e    = putStrLn (enemyName e ++ " with " ++ show (enemyHealth e) ++ " health")

dealDamageToEnemy :: Enemy -> Int -> Enemy
dealDamageToEnemy e amount = e { enemyHealth = enemyHealth e - amount }

isDead :: Enemy -> Bool
isDead e    = enemyHealth e <= 0

attackEnemy :: Player -> Enemy -> Enemy
attackEnemy p e    = e { enemyHealth = enemyHealth e - playerAttack }
    where
        playerAttack    = playerDamage p

updateEnemies :: List Enemy -> Enemy -> List Enemy
updateEnemies l new = updateList l new (\x -> enemyName x == enemyName new)

deleteDeadEnemies :: List Enemy -> List Enemy
deleteDeadEnemies (List e)  = List (filter (not . isDead) e)

enemyDamage :: Enemy -> Int
enemyDamage e   = weaponDamage (enemyWeapon e)

enemyAttackPlayer :: Enemy -> Player -> Player
enemyAttackPlayer e p  = p { playerHealth = playerHealth p - enemyDamage e }
