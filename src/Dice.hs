module Dice (
    randomDice
) where

import System.Random

randomDice :: Int -> IO Int
randomDice dice_type    = getStdRandom (randomR (1, dice_type))

