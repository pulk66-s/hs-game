module Move (
    tryMove
) where

import Game
import Room
import List
import Item
import Player

checkRoomKey :: Game -> (Int, Room) -> Direction -> Either String Game
checkRoomKey game (index, room) direction   = case key room of
    Just k  -> if hasKey (player game) k
        then Right newGame
        else Left "You don't have the key"
    Nothing -> Right newGame
    where
        newGame = updateRoom (saveCurrentRoom game) (index, room)

tryMove :: Game -> Direction -> Either String Game
tryMove game direction  = case getNextRoom game direction of
    Just r  -> checkRoomKey game r direction
    Nothing -> Left "No room in this direction"
