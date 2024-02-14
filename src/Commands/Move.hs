module Commands.Move (
    moveCommand
) where

import Game
import Room
import States.Fight

moveCommand :: Game -> Direction -> (Game -> IO()) -> IO()
moveCommand g d n = extractMaybe (moveRoom g d)
    where
        extractMaybe (Just g')  = putStr "Moving to " 
            >> printDirection d >> putStrLn "" >> checkFight g' n
        extractMaybe Nothing    = putStrLn "You can't go that way" >> n g
