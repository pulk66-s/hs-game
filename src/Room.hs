module Room (
    Room(..),
    Direction(..),
    enemyInRoom,
    findEnemy,
    printRoom,
    printDirection
) where

import Item
import Enemies

data Direction = North | East | South | West
    deriving (Show, Eq)

data Room = Room {
    nextRooms :: [(Direction, Room)],
    loot :: [Item],
    enemies :: [Enemy]
} deriving Show

enemyInRoom :: Room -> String -> Bool
enemyInRoom r n    = length (filter (\e -> enemyName e == n) (enemies r)) == 1

findEnemy :: Room -> String -> Maybe Enemy
findEnemy r n
    | enemyInRoom r n   = Just (head (filter (\e -> enemyName e == n) (enemies r)))
    | otherwise         = Nothing

printDirection :: Direction -> IO()
printDirection North    = putStr "North"
printDirection South    = putStr "South"
printDirection East     = putStr "East"
printDirection West     = putStr "West"

printDoorDirections :: [(Direction, Room)] -> IO()
printDoorDirections [] = putStrLn ""
printDoorDirections ((d, _):xs) = printDirection d >> putStrLn "" >> printDoorDirections xs

printDoors :: Room -> IO()
printDoors r    | null (nextRooms r)    = putStrLn "There are no doors"
                | otherwise             = putStrLn "There is doors:" >> printDoorDirections (nextRooms r)

printLoot :: Room -> IO()
printLoot r | null (loot r) = putStrLn "There is no loot"
            | otherwise     = printItems (loot r)

printEnemies :: Room -> IO()
printEnemies r  | null (enemies r) = putStrLn "There are no enemies"
                | otherwise         = listEnemies (enemies r)

printRoom :: Room -> IO()
printRoom r = printDoors r >> printLoot r >> printEnemies r
