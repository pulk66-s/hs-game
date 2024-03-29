module Room (
    Room(..),
    Direction(..),
    defaultRoom,
    addLootToRoom,
    addEnnemyToRoom,
    addNextRoom,
    printRoom,
    isNextRoom,
    printDirection,
    addKeyToRoom,
    roomHasEnemies,
    getEnemyByName,
    setWinRoom,
    opposite,
    grabItemFromRoom,
    deleteItemFromRoom
) where

import Item
import Item.Key
import Enemies
import List

data Direction = North | East | South | West
    deriving (Show, Eq)

data Room = Room {
    nextRooms :: List (Direction, Int),
    loot :: List Item,
    foundLoot :: List Item,
    enemies :: List Enemy,
    key :: Maybe Key,
    isWinRoom :: Bool
}

printDirection :: Direction -> IO()
printDirection North    = putStrLn "North"
printDirection East     = putStrLn "East"
printDirection South    = putStrLn "South"
printDirection West     = putStrLn "West"

defaultRoom :: Room
defaultRoom = Room (List []) (List []) (List []) (List []) Nothing False

addLootToRoom :: Item -> Room -> Room
addLootToRoom x r   = r { loot = addElem x (loot r) }

addEnnemyToRoom :: Enemy -> Room -> Room
addEnnemyToRoom x r = r { enemies = addElem x (enemies r) }

addNextRoom :: Direction -> Int -> Room -> Room
addNextRoom d i r   = r { nextRooms = addElem (d, i) (nextRooms r) }

printRoomLoot :: Room -> IO()
printRoomLoot r
    | isEmpty (loot r)  = putStr ""
    | otherwise         = putStrLn "There is some loot in there"

printRoomNextRooms :: Room -> IO()
printRoomNextRooms r
    | isEmpty (nextRooms r) = putStr ""
    | otherwise             = do
        putStrLn "There is some next rooms"
        printList (nextRooms r) (\(d, _) -> putStrLn ("Direction: " ++ show d))

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

roomHasEnemies :: Room -> Bool
roomHasEnemies r    = not (isEmpty (enemies r))

getEnemyByName :: Room -> String -> Maybe Enemy
getEnemyByName r name   = findInList (\e -> enemyName e == name) (enemies r)

setWinRoom :: Room -> Room
setWinRoom room = room { isWinRoom = True }

opposite :: Direction -> Direction
opposite North  = South
opposite East   = West
opposite South  = North
opposite West   = East

grabItemFromRoom :: String -> Room -> Maybe Item
grabItemFromRoom name room  = findInList (\i -> itemName i == name) (foundLoot room)

deleteItemFromRoom :: Item -> Room -> Room
deleteItemFromRoom item room    = room { foundLoot = removeList item (foundLoot room) }
