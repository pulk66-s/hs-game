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
    printDirection,
    addKeyToRoom
) where

import Item
import Enemies
import List

data Direction = North | East | South | West
    deriving (Show, Eq)

data Room = Room {
    nextRooms :: List (Direction, Int),
    loot :: List Item,
    enemies :: List Enemy,
    key :: Maybe Key
} deriving Show

printDirection :: Direction -> IO()
printDirection North    = putStrLn "North"
printDirection East     = putStrLn "East"
printDirection South    = putStrLn "South"
printDirection West     = putStrLn "West"

defaultRoom :: Room
defaultRoom = Room (List []) (List []) (List []) Nothing

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
isEnnemyWithName r n    = case findEnemyWithName r n of
    Just _  -> True
    Nothing -> False

printRoomLoot :: Room -> IO()
printRoomLoot r | isEmpty (loot r) = putStr ""
                | otherwise        = putStrLn "There is some loot in there"

printRoomNextRooms :: Room -> IO()
printRoomNextRooms r | isEmpty (nextRooms r) = putStr ""
                     | otherwise             = putStrLn "There is some next rooms"
    >> printList (nextRooms r) (\(d, _) -> putStrLn ("Direction: " ++ show d))

isNextRoom :: Room -> Direction -> Bool
isNextRoom r d  = case findInList (\(d', _) -> d' == d) (nextRooms r) of
    Just _  -> True
    Nothing -> False

printRoom :: Room -> IO()
printRoom r = do
    print "You are in a room"
    printRoomLoot r
    putStrLn ""
    printRoomNextRooms r

addKeyToRoom :: Key -> Room -> Room
addKeyToRoom k r    = r { key = Just k }
