module Item.Key (
    Key(..),
    printKey
) where

newtype Key = Key String
    deriving Show

printKey :: Key -> IO()
printKey (Key k) = putStr k
