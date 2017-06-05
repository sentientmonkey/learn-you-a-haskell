module Set
( fromList
, toList
, isEmpty
, size
, contains
, add
, remove
, map
) where

import Prelude hiding (map)
import qualified Data.List as List

newtype SetList a = SetList { getSetList :: [a] } deriving (Show, Eq)

fromList :: (Eq a) => [a] -> SetList a
fromList [] = SetList []
fromList (x:xs) = (add (fromList xs) x)

toList :: SetList a -> [a]
toList = getSetList

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

-- Note reverse needed because Eq not derived properly
-- see https://hackage.haskell.org/package/containers-0.5.10.2/docs/src/Data-Set-Internal.html#map
map :: (Eq a, Eq b) => (a -> b) -> SetList a -> SetList b
map f = fromList . reverse . List.map f . toList

-- functor, but doesn't wok because of fromList requires Eq and we can't change
-- fmap type signature (Data.Set has same problem)
instance Functor SetList where
    fmap f (SetList a) = SetList (fmap f a)
