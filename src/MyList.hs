module MyList (
    find,
    startsWith
) where

find :: (a -> Bool) -> [a] -> Maybe a
find _ []                   = Nothing
find f (x:xs)   | f x       = Just x
                | otherwise = find f xs

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith [] _                         = True
startsWith _ []                         = False
startsWith (x:xs) (y:ys)    | x == y    = startsWith xs ys
                            | otherwise = False
