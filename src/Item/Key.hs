module Item.Key (
    Key(..),
    printKey
) where

newtype Key = Key String
    

printKey :: Key -> IO()
printKey (Key k) = putStr k
