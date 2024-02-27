module States.Fight.Command (
    evaluateFightCommand
) where

import Game
import Terminal
import Enemies
import Room
import Player
import Player.Magic
import Player.Magic.SpellCast
import List
import Item
import Item.Weapon
import Item.Consumable
import Item.Key
import States.Fight.PlayerAttack
import States.Fight.EnemyAttack
import States.Fight.ItemUse

evaluateUnknownCommand :: Game -> IO Game
evaluateUnknownCommand game = do
    putStrLn "Command not found"
    return game

evaluateAttackCommand :: Game -> String -> IO Game
evaluateAttackCommand   = launchPlayerAttack

evaluateEnemyInfoCommand :: Game -> IO Game
evaluateEnemyInfoCommand game   = do
    listEnemies (enemies (getRoom game))
    return game

evaluatePlayerInfoCommand :: Game -> IO Game
evaluatePlayerInfoCommand game  = do
    printPlayer (player game)
    return game

evaluateUseItemFightCommand :: Game -> String -> IO Game
evaluateUseItemFightCommand game name   = do
    useItemInFight game name

evaluateUseSpellCommand :: Game -> String -> IO Game
evaluateUseSpellCommand = castSpell

evaluateFightCommand' :: Game -> FightCommand -> IO Game
evaluateFightCommand' game (Attack name)        = evaluateAttackCommand game name
evaluateFightCommand' game EnemyInfo            = evaluateEnemyInfoCommand game
evaluateFightCommand' game PlayerInfo           = evaluatePlayerInfoCommand game
evaluateFightCommand' game (UseItemFight name)  = evaluateUseItemFightCommand game name
evaluateFightCommand' game (UseSpell name)      = evaluateUseSpellCommand game name

evaluateFightCommand :: Game -> Maybe FightCommand -> IO Game
evaluateFightCommand game Nothing   = evaluateUnknownCommand game
evaluateFightCommand game (Just c)  = evaluateFightCommand' game c
