module Game (
    Game(..),
    defaultGame,
    getNextRoom,
    getRoom,
    updateRoom,
    saveCurrentRoom
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
startingRoom    = addEndRoom (addEnnemy (addLoot defaultRoom))
    where
        addLoot     = addNextRoom North 1
        addEnnemy   = addNextRoom South 2
        addEndRoom  = addNextRoom East 3

lootRoom :: Room
lootRoom    = addStartingRoom (addLoot defaultRoom)
    where
        addLoot r       = addLootToRoom (IConsumable littleHealthPotion) (addLootToRoom (IWeapon excalibur) r)
        addStartingRoom = addNextRoom South 0

enemyRoom :: Room
enemyRoom  = addLoot (addStartingRoom (addEnnemy defaultRoom))
    where
        addEnnemy       = addEnnemyToRoom goblin
        addStartingRoom = addNextRoom North 0
        addLoot         = addLootToRoom (IKey (Key "End Key"))

lockedRoom :: Room
lockedRoom = addStartingRoom (addKey defaultRoom)
    where
        addKey          = addKeyToRoom (Key "End Key")
        addStartingRoom = addNextRoom West 0

defaultRoomList :: List (Int, Room)
defaultRoomList = List [(0, startingRoom), (1, lootRoom), (2, enemyRoom), (3, lockedRoom)]

defaultGame :: Game
defaultGame = Game newPlayer (0, startingRoom) defaultRoomList

getNextRoom :: Game -> Direction -> Maybe (Int, Room)
getNextRoom g d = case findInList (\(d', _) -> d' == d) (nextRooms (getRoom g)) of
    Just (_, i) -> findInList (\(i', _) -> i' == i) (rooms g)
    Nothing     -> Nothing

getRoom :: Game -> Room
getRoom g = snd (room g)

updateRoom :: Game -> (Int, Room) -> Game
updateRoom game r   = game { room = r }

saveCurrentRoom :: Game -> Game
saveCurrentRoom game   = game { rooms = updateList (rooms game) (room game) (f (room game)) }
    where
        f (i, _) (i', _)    = i == i'
