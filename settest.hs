import Set
import Control.Monad
import System.Directory
import Test.HUnit

emptySetList = SetList []
setListWithMany = SetList [1,2,3]

tests = test [ "test1" ~: "isEmpty without element" ~: True ~=? (isEmpty emptySetList)
             , "test2" ~: "isEmpty with element" ~: False ~=? (isEmpty setListWithMany)
             , "test3" ~: "size without element" ~: 0 ~=? (size emptySetList)
             , "test4" ~: "size with element" ~: 3 ~=? (size setListWithMany)
             , "test5" ~: "contains on empty" ~: False ~=? (contains emptySetList 1)
             , "test6" ~: "contains with items" ~: True ~=? (contains setListWithMany 1)
             , "test7" ~: "add element to empty set" ~: SetList [1] ~=? (add emptySetList 1)
             , "test8" ~: "add existing element to set" ~: SetList [1,2,3] ~=? (add setListWithMany 1)
             , "test9" ~: "remove element from set" ~: SetList [1,3] ~=? (remove setListWithMany 2)
             ]


