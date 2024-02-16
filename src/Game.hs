module Game (
    Game(..),
    defaultGame,
    getNextRoom,
    getRoom
) where

import Player
import Room
import Enemies
import Item
import List

data Game = Game {
    player :: Player,
    room :: (Int, Room),
    rooms :: List (Int, Room)
} deriving Show

startingRoom :: Room
startingRoom    = addEnnemy (addLoot defaultRoom)
    where
        addLoot     = addNextRoom North 1
        addEnnemy   = addNextRoom South 2

lootRoom :: Room
lootRoom    = addStartingRoom (addLoot defaultRoom)
    where
        addLoot         = addLootToRoom (IWeapon excalibur)
        addStartingRoom = addNextRoom South 0

enemyRoom :: Room
enemyRoom  = addStartingRoom (addEnnemy defaultRoom)
    where
        addEnnemy       = addEnnemyToRoom goblin
        addStartingRoom = addNextRoom North 0

defaultRoomList :: List (Int, Room)
defaultRoomList = List [(0, startingRoom), (1, lootRoom), (2, enemyRoom)]

defaultGame :: Game
defaultGame = Game newPlayer (0, startingRoom) defaultRoomList

getNextRoom :: Game -> Direction -> Maybe (Int, Room)
getNextRoom g d = case findInList (\(d', _) -> d' == d) (nextRooms (getRoom g)) of
    Just (_, i) -> findInList (\(i', _) -> i' == i) (rooms g)
    Nothing     -> Nothing

getRoom :: Game -> Room
getRoom g = snd (room g)
