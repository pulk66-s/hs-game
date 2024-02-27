module Enemies (
    EnemyStatistic(..),
    Enemy(..),
    goblin,
    listEnemies,
    printEnemy,
    dealDamageToEnemy,
    updateEnemies,
    attackEnemy,
    deleteDeadEnemies,
    enemyAttackPlayer,
    enemyDamage,
    enemyIsDead,
    enemyStrengthStat
) where

import Item.Weapon
import List
import Player.Data

data EnemyStatistic = EnemyStatistic {
    enemyStrength :: Int,
    enemyDexterity :: Int,
    enemyIntelligence :: Int
} 

data Enemy = Enemy {
    enemyName :: String,
    enemyHealth :: Int,
    enemyWeapon :: Weapon,
    enemyStatistic :: EnemyStatistic
}

defaultEnemyStatistic :: EnemyStatistic
defaultEnemyStatistic  = EnemyStatistic 10 10 10

goblin :: Enemy
goblin  = Enemy "Goblin" 15 rustySword defaultEnemyStatistic

listEnemies :: List Enemy -> IO()
listEnemies (List [])   = putStrLn "There is no Enemy"
listEnemies e           = printList e printEnemy

printEnemy :: Enemy -> IO()
printEnemy e    = putStrLn (enemyName e ++ " with " ++ show (enemyHealth e) ++ " health")

dealDamageToEnemy :: Enemy -> Int -> Enemy
dealDamageToEnemy e amount = e { enemyHealth = enemyHealth e - amount }

enemyIsDead :: Enemy -> Bool
enemyIsDead e    = enemyHealth e <= 0

playerDamage :: Player -> Int
playerDamage p  = getDmg (playerWeapon p)
    where
        getDmg Nothing  = 1
        getDmg (Just w) = weaponDamage w

attackEnemy :: Player -> Enemy -> Enemy
attackEnemy p e    = e { enemyHealth = enemyHealth e - playerAttack }
    where
        playerAttack    = playerDamage p

updateEnemies :: List Enemy -> Enemy -> List Enemy
updateEnemies l new = updateList l new (\x -> enemyName x == enemyName new)

deleteDeadEnemies :: List Enemy -> List Enemy
deleteDeadEnemies (List e)  = List (filter (not . enemyIsDead) e)

enemyDamage :: Enemy -> Int
enemyDamage e   = weaponDamage (enemyWeapon e)

enemyAttackPlayer :: Enemy -> Player -> Player
enemyAttackPlayer e p  = p { playerHealth = playerHealth p - enemyDamage e }

enemyStrengthStat :: Enemy -> Int
enemyStrengthStat enemy = enemyStrength (enemyStatistic enemy)
