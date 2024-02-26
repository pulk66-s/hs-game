module States.Fight (
    checkFight
) where

import Game
import Room
import Enemies
import Player
import Terminal
import Item
import List
import Dice

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

launchAttack :: Game -> String -> IO Game
launchAttack game name  = do
    diceRoll    <- randomDice 20
    putStrLn ("You rolled a " ++ show diceRoll)
    if diceRoll >= strengthStat (player game)
        then launchAttackSuccess game name
        else launchAttackFailure game

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

useItemInFight :: Game -> String -> IO Game
useItemInFight game name    = case findInList nameFilter (inventory (player game)) of
    Just item   -> do
        checkItemUsage game (playerUseItemInFight (player game) item)
        case enemies (getRoom game) of
            List [] -> return game
            List e  -> do
                putStrLn "Enemies are attacking you"
                enemiesAttack game e
    Nothing     -> do
        putStrLn "Item not found"
        return game
    where
        nameFilter (IWeapon (WSword s))         = swordName s == name
        nameFilter (IConsumable (CHealth p))    = healthPotionName p == name
        nameFilter (IKey (Key k))               = k == name
        checkItemUsage game' (Just p)   = do
            putStrLn ("You used " ++ name ++ " item")
            return game' { player = p }
        checkItemUsage game' Nothing     = do
            putStrLn "You can't use this item"
            return game'

evaluateFightCommand :: Game -> Maybe FightCommand -> IO Game
evaluateFightCommand game Nothing                       = do
    putStrLn "Command not found"
    return game
evaluateFightCommand game (Just (Attack name))      = launchAttack game name
evaluateFightCommand game (Just EnemyInfo)              = do
    listEnemies (enemies (getRoom game))
    return game
evaluateFightCommand game (Just PlayerInfo)             = do
    printPlayer (player game)
    return game
evaluateFightCommand game (Just (UseItemFight name))    = useItemInFight game name

fightLoop :: Game -> IO Game
fightLoop game  = case enemies (getRoom game) of
    List [] -> do
        putStrLn "You killed all enemies"
        return game
    List _  -> do
        putStrLn "\nWhat do you want to do ?"
        cmd     <- getFightCommand
        game'   <- evaluateFightCommand game cmd
        fightLoop game'

checkFight :: Game -> (Game -> IO()) -> IO()
checkFight game next    = case enemies (getRoom game) of
    List [] -> next game
    List e  -> do
        putStrLn "You are attacked !"
        putStrLn "Here are the enemies:"
        listEnemies (List e)
        game'   <- fightLoop game
        next game'
