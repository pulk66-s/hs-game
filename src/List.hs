module List (
    List(..),
    removeList,
    addList,
    printList,
    findInList,
    defaultList,
    isEmpty,
    updateList,
    lengthList,
    addElem,
    startsWith,
    find,
    map',
    unwrapList,
    getFirstWord
) where

newtype List a = List [a] 

defaultList :: List a
defaultList = List []

addElem :: a -> List a -> List a
addElem x (List xs) = List (x:xs)

addList :: List a -> List a -> List a
addList (List xs) (List ys) = List (xs ++ ys)

removeList :: Eq a => a -> List a -> List a
removeList x (List xs) = List (filter (/= x) xs)

printList :: List a -> (a -> IO()) -> IO()
printList (List []) _   = putStrLn "There is no elements"
printList (List xs) f   = mapM_ f xs

findInList :: (a -> Bool) -> List a -> Maybe a
findInList f (List xs) = unwrapFilter (filter f xs)
    where
        unwrapFilter []     = Nothing
        unwrapFilter (x:_)  = Just x

isEmpty :: List a -> Bool
isEmpty (List []) = True
isEmpty _         = False

updateList :: List a -> a -> (a -> Bool) -> List a
updateList (List xs) new eq = List (map (checkElem eq new) xs)
    where
        checkElem f n x
            | f x       = n
            | otherwise = x

lengthList :: List a -> Int
lengthList (List xs) = length xs

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith [] _                         = True
startsWith _ []                         = False
startsWith (x:xs) (y:ys)    | x == y    = startsWith xs ys
                            | otherwise = False

find :: (a -> Bool) -> [a] -> Maybe a
find _ []                   = Nothing
find f (x:xs)   | f x       = Just x
                | otherwise = find f xs

map' :: (a -> b) -> List a -> List b
map' f (List xs) = List (map f xs)

unwrapList :: List a -> [a]
unwrapList (List a) = a

getFirstWord :: String -> String
getFirstWord []     = []
getFirstWord (x:xs)
    | x == ' '      = []
    | otherwise     = x : getFirstWord xs
