module States.Fight.EnemyAttack (
    enemiesAttack
) where

import Game
import Enemies
import Dice

-- Because we can't use other libraries...
foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM _ a []        = return a
foldM f a (x:xs)    = do
    newA <- f a x
    foldM f newA xs

checkStrengthRoll :: Int -> Enemy -> Game -> IO Game
checkStrengthRoll diceRoll enemy game
    | diceRoll >= enemyStrengthStat enemy   = do
        putStrLn ("Enemy " ++ enemyName enemy ++ " hit you for " ++ show (enemyDamage enemy) ++ " damage")
        return (game { player = enemyAttackPlayer enemy (player game) })
    | otherwise                             = do
        putStrLn ("Enemy " ++ enemyName enemy ++ " missed")
        return game

enemyAttack :: Game -> Enemy -> IO Game
enemyAttack game enemy  = do
    diceRoll    <- randomDice 20
    putStrLn ("Enemy " ++ enemyName enemy ++ " rolled a " ++ show diceRoll)
    checkStrengthRoll diceRoll enemy game

enemiesAttack :: Game -> [Enemy] -> IO Game
enemiesAttack   = foldM enemyAttack
