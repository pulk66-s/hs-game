module Move (
    tryMove
) where

import Game
import Room
import Player

checkRoomKey :: Game -> (Int, Room) -> Either String Game
checkRoomKey game (index, r)    = case key r of
    Just k  -> checkKey game k
    Nothing -> Right newGame
    where
        newGame                         = updateRoom (saveCurrentRoom game) (index, r)
        checkKey g k
            | hasKey (player g) k   = Right newGame
            | otherwise             = Left "You don't have the key"

tryMove :: Game -> Direction -> Either String Game
tryMove game direction  = case getNextRoom game direction of
    Just r  -> checkRoomKey game r
    Nothing -> Left "No room in this direction"
