module Item.Key (
    Key(..),
    printKey,
    keyName
) where

newtype Key = Key String

printKey :: Key -> IO()
printKey (Key k) = putStr k

keyName :: Key -> String
keyName (Key k) = k

instance Eq Key where
    (==) (Key k1) (Key k2)  = k1 == k2
