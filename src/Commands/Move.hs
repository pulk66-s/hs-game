module Commands.Move (
    moveCommand
) where

import Game
import Room

moveCommand :: Game -> Direction -> (Game -> IO()) -> IO()
moveCommand g d n = extractMaybe (moveRoom g d)
    where
        extractMaybe (Just g')  = print ("Moving to " ++ show d) >> n g'
        extractMaybe Nothing    = print "You can't go that way" >> n g
