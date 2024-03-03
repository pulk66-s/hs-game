module Commands.Search (
    searchCommand
) where

import Game
import Room
import Item
import List

applyChanges :: Game -> Game
applyChanges g  = newRoom g
    where
        addFoundLoot (i, r) = (i, r { foundLoot = loot r })
        newRoom g'          = g' { room = deleteLoot (addFoundLoot (room g)) }
        deleteLoot (i, r)   = (i, r { loot = defaultList })

searchCommand :: Game -> (Game -> IO()) -> IO()
searchCommand g n   = case loot (getRoom g) of
    List [] -> putStrLn "There is nothing to loot" >> n g
    List l  -> do
        putStrLn "You found some loot"
        printItems l
        putStrLn ""
        n (applyChanges g)
