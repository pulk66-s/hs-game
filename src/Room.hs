module Room (
    Room(..),
    Direction(..),
    defaultRoom,
    addLootToRoom,
    addEnnemyToRoom,
    addNextRoom,
    isEnemies,
    isEnnemyWithName,
    printRoom,
    findEnemyWithName,
    isNextRoom,
    printDirection
) where

import Item
import Enemies
import List
import Maybe

data Direction = North | East | South | West
    deriving (Show, Eq)

data Room = Room {
    nextRooms :: List (Direction, Int),
    loot :: List Item,
    enemies :: List Enemy
} deriving Show

printDirection :: Direction -> IO()
printDirection North    = putStrLn "North"
printDirection East     = putStrLn "East"
printDirection South    = putStrLn "South"
printDirection West     = putStrLn "West"

defaultRoom :: Room
defaultRoom = Room (List []) (List []) (List [])

addLootToRoom :: Item -> Room -> Room
addLootToRoom x r = r { loot = addElem x (loot r) }

addEnnemyToRoom :: Enemy -> Room -> Room
addEnnemyToRoom x r   = r { enemies = addElem x (enemies r) }

addNextRoom :: Direction -> Int -> Room -> Room
addNextRoom d i r   = r { nextRooms = addElem (d, i) (nextRooms r) }

isEnemies :: Room -> Bool
isEnemies r = not (isEmpty (enemies r))

findEnemyWithName :: Room -> String -> Maybe Enemy
findEnemyWithName r name    = findInList (\x -> enemyName x == name) (enemies r)

isEnnemyWithName :: Room -> String -> Bool
isEnnemyWithName r n    = isJust (findEnemyWithName r n)

printRoomLoot :: Room -> IO()
printRoomLoot r | isEmpty (loot r) = putStr ""
                | otherwise        = putStrLn "There is some loot in there"

printRoomNextRooms :: Room -> IO()
printRoomNextRooms r | isEmpty (nextRooms r) = putStr ""
                     | otherwise             = putStrLn "There is some next rooms"
    >> printList (nextRooms r) (\(d, _) -> putStrLn ("Direction: " ++ show d))

isNextRoom :: Room -> Direction -> Bool
isNextRoom r d  = isJust (findInList (\(d', _) -> d' == d) (nextRooms r))

printRoom :: Room -> IO()
printRoom r = print "You are in a room"
    >> printRoomLoot r
    >> putStrLn ""
    >> printRoomNextRooms r
    