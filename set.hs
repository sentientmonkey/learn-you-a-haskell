module Set
( SetList(..)
, isEmpty
, size
, contains
, add
, remove
) where

newtype SetList a = SetList { getSetList :: [a] } deriving (Eq, Show)

isEmpty :: SetList a -> Bool
isEmpty (SetList []) = True
isEmpty (SetList x) = False

size :: SetList a -> Int
size (SetList []) = 0
size (SetList(_:xs)) = 1 + size (SetList xs)

contains :: (Eq a) => SetList a -> a -> Bool
contains (SetList(y:ys)) x
    | x==y = True
    | otherwise = contains (SetList ys) x
contains (SetList []) x = False

add :: (Eq a) => SetList a -> a -> SetList a
add (SetList xs) x
    | contains (SetList xs) x = (SetList xs)
    | otherwise = SetList (xs ++ [x])

remove :: (Eq a) => SetList a -> a -> SetList a
remove (SetList []) _ = SetList []
remove (SetList (y:ys)) x
    | x==y = (SetList ys)
    | otherwise = SetList([y] ++ getSetList (remove (SetList ys) x))
