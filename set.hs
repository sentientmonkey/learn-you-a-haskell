module Set
( isEmpty
, size
, contains
, add
, remove
) where

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty x = False

size :: [a] -> Int
size [] = 0
size (_:xs) = 1 + size xs

contains :: (Eq a) => [a] -> a -> Bool
contains (y:ys) x
    | x==y = True
    | otherwise = contains ys x
contains [] x = False

add :: (Eq a) => [a] -> a -> [a]
add xs x
    | contains xs x = xs
    | otherwise = xs ++ [x]

remove :: (Eq a) => [a] -> a -> [a]
remove [] _ = []
remove (y:ys) x
    | x==y = ys
    | otherwise = [y] ++ remove ys x
