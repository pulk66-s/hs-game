module Commands.Search (
    searchCommand
) where

import Game
import Room

searchCommand :: Game -> (Game -> IO()) -> IO()
searchCommand g n   | null (loot (room g))  = print "You find nothing" >> n g
                    | otherwise             = print ("You find " ++ show (length items) ++ " items")
    >> printItemList items >> n (searchRoom g)
    where
        items   = loot (room g)

