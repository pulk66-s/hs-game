module States.Fight.EnemyAttack (
    enemiesAttack
) where

import Game
import Enemies
import Dice

enemiesAttack :: Game -> [Enemy] -> IO Game
enemiesAttack game enemies  = enemiesDices game enemies
    where
        enemiesDices game []        = return game
        enemiesDices game (x:xs)    = do
            diceRoll    <- randomDice 20
            putStrLn ("Enemy " ++ enemyName x ++ " rolled a " ++ show diceRoll)
            if diceRoll >= enemyStrengthStat x
                then do
                    putStrLn ("Enemy " ++ enemyName x ++ " hit you")
                    enemiesDices (game { player = enemyAttackPlayer x (player game) }) xs
                else do
                    putStrLn ("Enemy " ++ enemyName x ++ " missed his attack")
                    enemiesDices game xs
