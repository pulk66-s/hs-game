module List (
    List(..),
    removeList,
    addList,
    printList,
    findInList,
    defaultList,
    isEmpty,
    updateList,
    lengthList
) where

newtype List a = List [a] deriving Show

defaultList :: List a
defaultList = List []

addList :: a -> List a -> List a
addList x (List xs) = List (x:xs)

removeList :: Eq a => a -> List a -> List a
removeList x (List xs) = List (filter (/= x) xs)

printList :: List a -> (a -> IO()) -> IO()
printList (List []) _   = putStrLn "There is no elements in the List"
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
