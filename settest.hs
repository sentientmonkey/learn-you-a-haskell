import Set
import Control.Monad
import System.Directory
import Test.HUnit

emptySet = []
setWithMany = [1,2,3]

tests = test [ "test1" ~: "isEmpty without element" ~: True ~=? (isEmpty emptySet)
             , "test2" ~: "isEmpty with element" ~: False ~=? (isEmpty setWithMany)
             , "test3" ~: "size without element" ~: 0 ~=? (size emptySet)
             , "test4" ~: "size with element" ~: 3 ~=? (size setWithMany)
             , "test5" ~: "contains on empty" ~: False ~=? (contains emptySet 1)
             , "test6" ~: "contains with items" ~: True ~=? (contains setWithMany 1)
             , "test7" ~: "add element to empty set" ~: [1] ~=? (add emptySet 1)
             , "test8" ~: "add existing element to set" ~: [1,2,3] ~=? (add setWithMany 1)
             , "test9" ~: "remove element from set" ~: [1,3] ~=? (remove setWithMany 2)
             ]


