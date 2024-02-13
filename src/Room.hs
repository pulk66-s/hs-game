module Room (
    Room(..),
    Direction(..),
    testRooms,
    printItemList,
    enemyInRoom,
    findEnemy
) where

import Item
import Enemies
import Player

data Direction = North | East | South | West
    deriving (Show, Eq)

data Room = Room {
    nextRooms :: [(Direction, Room)],
    loot :: [Item],
    enemies :: [Enemy]
} deriving Show

testRooms :: Room
testRooms   = Room rooms [] []
    where
        rooms       = [(North, lootRoom), (South, enemyRoom), (East, emptyRoom), (West, emptyRoom)]
        emptyRoom   = Room [] [] []
        lootRoom    = Room [] [IWeapon excalibur] []
        enemyRoom   = Room [] [] [goblin]

printItemList :: [Item] -> IO()
printItemList l  = print (show l)

enemyInRoom :: Room -> String -> Bool
enemyInRoom r n    = length (filter (\e -> enemyName e == n) (enemies r)) == 1

findEnemy :: Room -> String -> Maybe Enemy
findEnemy r name
    | enemyInRoom r name   = Just (head (filter (\e -> enemyName e == name) (enemies r)))
    | otherwise             = Nothing
