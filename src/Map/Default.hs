module Map.Default (
    defaultRoom
) where

import Room
import Item
import Enemies

itemRoom :: Room
itemRoom    = Room [(South, startingRoom)] [IWeapon excalibur] []

mobRoom :: Room
mobRoom     = Room [(North, startingRoom)] [] [goblin]

startingRoom :: Room
startingRoom    = Room [(North, itemRoom), (South, mobRoom)] [] []

defaultRoom :: Room
defaultRoom = startingRoom