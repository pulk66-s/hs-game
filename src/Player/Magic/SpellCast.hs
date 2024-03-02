module Player.Magic.SpellCast (
    castSpell,
    getSpell
) where

import Game
import Player.Data
import States.Fight.PlayerAttack
import Enemies
import Room
import List

getSpell :: Player -> String -> Maybe PlayerSpell
getSpell p name = do
    let (List inv)  = spellInventory (playerMagic p)
    checkCategories inv
    where
        spellNameIs _ spell                     = spellName spell == name
        checkCategories []                      = Nothing
        checkCategories ((_, List spells):xs)   = case findInList (spellNameIs name) (List spells) of
            Nothing     -> checkCategories xs
            Just spell  -> Just spell

applyDamages :: PlayerSpell -> Enemy -> Enemy
applyDamages spell enemy    = dealDamageToEnemy enemy (spellDamage spell)

removePlayerMana :: Game -> PlayerSpell -> IO Game
removePlayerMana game spell = do
    return game {
        player = (player game) {
            playerMagic = (playerMagic (player game)) {
                playerMana = playerMana (playerMagic (player game)) - spellManaCost spell
            }
        }
    }

checkSpellTarget :: Game -> String -> PlayerSpell -> IO Game
checkSpellTarget game name spell    = case getEnemyByName (getRoom game) name of
    Nothing     -> do
        putStrLn "Enemy not found"
        return game
    Just enemy  -> do
        let damagedEnemy    = applyDamages spell enemy
        game'               <- removePlayerMana game spell
        if enemyIsDead damagedEnemy then putStrLn ("You killed " ++ name)
        else putStrLn ("You attacked " ++ name)
        let updatedEnemies  = updateEnemies (enemies (getRoom game')) damagedEnemy
        let aliveEnemies    = deleteDeadEnemies updatedEnemies
        let (index, r)      = room game'
        return game' { room = (index, r { enemies = aliveEnemies }) }

getSpellTarget :: Game -> String -> PlayerSpell -> IO Game
getSpellTarget game name spell  = case cutName of
    ""  -> do
        putStrLn "You need to specify a target"
        return game
    _   -> do
        game'   <- checkSpellTarget game cutName spell
        checkStillEnemies game' (enemies (getRoom game'))
    where
        cutName = drop (length (getFirstWord name) + 1) name

castSpell :: Game -> String -> IO Game
castSpell game name = case getSpell (player game) (getFirstWord name) of
    Nothing     -> do
        putStrLn ("Spell not found(" ++ getFirstWord name ++ ")")
        return game
    Just spell  -> getSpellTarget game name spell
