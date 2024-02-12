module Room (
    Room(..),
    Direction(..),
    testRooms,
    printItemList
) where

import Item

data Direction = North | East | South | West
    deriving (Show, Eq)

data Room = Room {
    nextRooms :: [(Direction, Room)],
    loot :: [Item]
} deriving Show

testRooms :: Room
testRooms   = Room rooms []
    where
        rooms   = [(North, lootRoom), (South, emptyRoom), (East, emptyRoom), (West, emptyRoom)]
        emptyRoom   = Room [] []
        lootRoom    = Room [] [Item (Weapon (Sword "Excalibur" 10))]

printItemList :: [Item] -> IO()
printItemList l  = print (show l)
