module Game (
    Game(..),
    moveRoom
) where

import Player
import Room
import MyList

data Game = Game {
    player :: Player,
    room :: Room
} deriving Show

moveRoom :: Game -> Direction -> Maybe Game
moveRoom g d    = extractMaybe (find (\(d', _) -> d == d') (nextRooms (room g)))
    where
        extractMaybe (Just (_, r))  = Just (g { room = r })
        extractMaybe Nothing      = Nothing
