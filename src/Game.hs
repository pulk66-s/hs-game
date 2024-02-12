module Game (
    Game(..),
    moveRoom,
    searchRoom
) where

import Player
import Room
import MyList
import Item

data Game = Game {
    player :: Player,
    room :: Room
} deriving Show

moveRoom :: Game -> Direction -> Maybe Game
moveRoom g d    = extractMaybe (find (\(d', _) -> d == d') (nextRooms (room g)))
    where
        extractMaybe (Just (_, r))  = Just (g { room = r })
        extractMaybe Nothing      = Nothing

addItems :: Player -> [Item] -> Player
addItems p items    = p { inventory = inventory p ++ items }

searchRoom :: Game -> Game
searchRoom (Game p r)   = Game (addItems p (loot r)) (r { loot = [] })