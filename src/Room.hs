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
    findEnemyWithName
) where

import Item
import Enemies
import List

data Direction = North | East | South | West
    deriving (Show, Eq)

data Room = Room {
    nextRooms :: List (Direction, Int),
    loot :: List Item,
    enemies :: List Enemy
} deriving Show

defaultRoom :: Room
defaultRoom = Room (List []) (List []) (List [])

addLootToRoom :: Item -> Room -> Room
addLootToRoom x r = r { loot = addList x (loot r) }

addEnnemyToRoom :: Enemy -> Room -> Room
addEnnemyToRoom x r   = r { enemies = addList x (enemies r) }

addNextRoom :: Direction -> Int -> Room -> Room
addNextRoom d i r   = r { nextRooms = addList (d, i) (nextRooms r) }

isEnemies :: Room -> Bool
isEnemies r = not (isEmpty (enemies r))

findEnemyWithName :: Room -> String -> Maybe Enemy
findEnemyWithName r name    = findInList (\x -> enemyName x == name) (enemies r)

isEnnemyWithName :: Room -> String -> Bool
isEnnemyWithName r n    = isJust (findEnemyWithName r n)
    where
        isJust (Just _) = True
        isJust _        = False

printRoom :: Room -> IO()
printRoom r = do
    putStrLn "You are in a room with the following items:"
    printList (loot r) printItem
    putStrLn "You can go to the following rooms:"
    printList (nextRooms r) (\(d, i) -> putStrLn (show d ++ " to room " ++ show i))
    if isEnemies r
        then putStrLn "There are enemies in this room"
        else putStrLn "There are no enemies in this room"
    putStrLn ""
