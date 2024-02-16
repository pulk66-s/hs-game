module Commands.Move (
    moveCommand
) where

import Game
import Room
import States.Fight
import List

saveCurrentRoom :: Game -> Game
saveCurrentRoom g   = g { rooms = updateList (rooms g) (i, r) (\(i', _) -> i' == i) }
    where
        (i, r) = room g

moveCommand :: Game -> Direction -> (Game -> IO()) -> IO()
moveCommand g d n   = case getNextRoom g d of
    Just r  -> do
        putStrLn ("Moving to " ++ show d)
        checkFight ((saveCurrentRoom g) { room = r }) n
    Nothing -> putStrLn "You can't go that way" >> n g
