module Player.Magic (
    defaultPlayerMagic
) where

import Player.Data
import List

fireball :: PlayerSpell
fireball    = PlayerSpell "Fireball" 20 3

acidArrow :: PlayerSpell
acidArrow   = PlayerSpell "Acid_Arrow" 15 2

magicMissile :: PlayerSpell
magicMissile    = PlayerSpell "Magic_Missile" 10 1

level1Spells :: List PlayerSpell
level1Spells    = List [magicMissile]

level2Spells :: List PlayerSpell
level2Spells    = List [acidArrow]

level3Spells :: List PlayerSpell
level3Spells    = List [fireball]

defaultPlayerMagic :: PlayerMagic
defaultPlayerMagic  = PlayerMagic 10 10 (List [
    (1, level1Spells), (2, level2Spells), (3, level3Spells)
    ])

