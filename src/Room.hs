module Room (
    Room(..),
    Direction(..),
    testRooms
) where

data Direction = North | East | South | West
    deriving (Show, Eq)

newtype Room = Room {
    nextRooms :: [(Direction, Room)]
} deriving Show

testRooms :: Room
testRooms   = Room rooms
    where
        rooms   = [(North, emptyRoom), (South, emptyRoom), (East, emptyRoom), (West, emptyRoom)]
        emptyRoom   = Room []
