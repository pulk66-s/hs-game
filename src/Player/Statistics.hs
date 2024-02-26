module Player.Statistics (
    defaultPlayerStatistic,
    strengthStat
) where

import Player.Data

defaultPlayerStatistic :: PlayerStatistic
defaultPlayerStatistic  = PlayerStatistic 10 10 10

strengthStat :: Player -> Int
strengthStat p  = playerStrength (playerStatistic p)
