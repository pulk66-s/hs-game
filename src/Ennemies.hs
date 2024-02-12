module Ennemies (
    Ennemy(..),
    goblin
) where

import Item

data Ennemy = Ennemy {
    name :: String,
    health :: Int,
    weapon :: Weapon
} deriving Show

goblin :: Ennemy
goblin  = Ennemy "Goblin" 10 rustySword