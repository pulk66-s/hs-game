module Commands.Move (
    moveCommand
) where

import Game
import Room
import States.Fight
import Move

moveCommand :: Game -> Direction -> (Game -> IO()) -> IO()
moveCommand game direction next = case tryMove game direction of
    Left err    -> putStrLn err >> next game
    Right g     -> checkFight g next
