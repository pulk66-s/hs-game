module Player.Data (
    Player(..),
    PlayerStatistic(..)
) where

import Item
import Item.Weapon
import List

data PlayerStatistic = PlayerStatistic {
    playerStrength :: Int,
    playerDexterity :: Int,
    playerIntelligence :: Int
} 

data Player = Player {
    playerHealth :: Int,
    playerMaxHealth :: Int,
    playerName :: String,
    playerWeapon :: Maybe Weapon,
    inventory :: List Item,
    playerStatistic :: PlayerStatistic
} 
