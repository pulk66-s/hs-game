module Commands.Search (
    searchCommand
) where

import Game
import Room
import Item
import List
import Player

applyChanges :: Game -> Game
applyChanges g  = updateRoom (updatePlayer g)
    where
        updatePlayer g      = g { player = updateInventory (player g) }
        updateInventory p   = p { inventory = addList (inventory p) (loot (getRoom g)) }
        updateRoom g        = g { room = updateLoot (room g) }
        updateLoot (i, r)   = (i, r { loot = defaultList })

searchCommand :: Game -> (Game -> IO()) -> IO()
searchCommand g n   = case loot (getRoom g) of
    List [] -> putStrLn "There is nothing to loot" >> n g
    List l  -> do
        putStrLn "You found some loot"
        printItems l
        n (applyChanges g)
