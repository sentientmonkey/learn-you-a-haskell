module PhoneBook
( PhoneNumber
, PhoneBook
, inPhoneBook
) where

import qualified Data.Map as Map  

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool  
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook

type AssocList k v = [(k,v)]  

type IntMap = Map.Map Int
