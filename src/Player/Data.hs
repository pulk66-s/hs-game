module Player.Data (
    Player(..),
    PlayerStatistic(..),
    PlayerMagic(..),
    PlayerSpell(..)
) where

import Item
import Item.Weapon
import List

data PlayerStatistic = PlayerStatistic {
    playerStrength :: Int,
    playerDexterity :: Int,
    playerIntelligence :: Int
}

data PlayerSpell = PlayerSpell {
    spellName :: String,
    spellDamage :: Int,
    spellManaCost :: Int
}

data PlayerMagic = PlayerMagic {
    playerMana :: Int,
    playerMaxMana :: Int,
    spellInventory :: List (Int, List PlayerSpell)
}

data Player = Player {
    playerHealth :: Int,
    playerMaxHealth :: Int,
    playerName :: String,
    playerWeapon :: Maybe Weapon,
    inventory :: List Item,
    playerStatistic :: PlayerStatistic,
    playerMagic :: PlayerMagic
} 
