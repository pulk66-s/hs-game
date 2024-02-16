module Game (
    Game(..),
    defaultGame
) where

import Player
import Room
import Enemies
import MyList
import Item
import List

data Game = Game {
    player :: Player,
    room :: Room,
    rooms :: List (Int, Room)
} deriving Show

startingRoom :: Room
startingRoom    = addEnnemy (addLoot defaultRoom)
    where
        addLoot     = addNextRoom North 1
        addEnnemy   = addNextRoom South 2

lootRoom :: Room
lootRoom    = addLoot defaultRoom
    where
        addLoot = addLootToRoom (IWeapon excalibur)

enemyRoom :: Room
enemyRoom  = addEnnemy defaultRoom
    where
        addEnnemy = addEnnemyToRoom goblin

defaultRoomList :: List (Int, Room)
defaultRoomList = List [(0, startingRoom), (1, lootRoom), (2, enemyRoom)]

defaultGame :: Game
defaultGame = Game newPlayer defaultRoom defaultRoomList
