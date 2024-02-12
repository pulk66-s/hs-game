module Room (
    Room(..),
    Direction(..),
    testRooms,
    printItemList
) where

import Item
import Ennemies

data Direction = North | East | South | West
    deriving (Show, Eq)

data Room = Room {
    nextRooms :: [(Direction, Room)],
    loot :: [Item],
    ennemies :: [Ennemy]
} deriving Show

testRooms :: Room
testRooms   = Room rooms [] []
    where
        rooms       = [(North, lootRoom), (South, enemyRoom), (East, emptyRoom), (West, emptyRoom)]
        emptyRoom   = Room [] [] []
        lootRoom    = Room [] [Item excalibur] []
        enemyRoom   = Room [] [] [goblin]

printItemList :: [Item] -> IO()
printItemList l  = print (show l)
