module Maybe (
    isJust,
    isFalse
) where

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _        = False

isFalse :: Maybe a -> Bool
isFalse (Just _) = False
isFalse _        = True
