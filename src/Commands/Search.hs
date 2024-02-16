module Commands.Search (
    searchCommand
) where

import Game
import Room
import Item

searchCommand :: Game -> (Game -> IO()) -> IO()
searchCommand g n = print ""
-- searchCommand g n   | null (loot (room g))  = putStrLn "You find nothing" >> n g
--                     | otherwise             = putStrLn ("You find " ++ show (length items) ++ " items")
--     >> printItems items >> n (searchRoom g)
--     where
--         items   = loot (room g)

