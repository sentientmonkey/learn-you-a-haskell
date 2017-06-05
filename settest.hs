import qualified Set
import Control.Monad
import System.Exit
import Test.HUnit

emptySetList = Set.fromList []
setListWithMany = Set.fromList [1,2,3]

tests = test [ "test1" ~: "isEmpty without element" ~: True ~=? (Set.isEmpty emptySetList)
             , "test2" ~: "isEmpty with element" ~: False ~=? (Set.isEmpty setListWithMany)
             , "test3" ~: "size without element" ~: 0 ~=? (Set.size emptySetList)
             , "test4" ~: "size with element" ~: 3 ~=? (Set.size setListWithMany)
             , "test5" ~: "contains on empty" ~: False ~=? (Set.contains emptySetList 1)
             , "test6" ~: "contains with items" ~: True ~=? (Set.contains setListWithMany 1)
             , "test7" ~: "add element to empty set" ~: Set.fromList [1] ~=? (Set.add emptySetList 1)
             , "test8" ~: "add existing element to set" ~: Set.fromList [1,2,3] ~=? (Set.add setListWithMany 1)
             , "test9" ~: "remove element from set" ~: Set.fromList [1,3] ~=? (Set.remove setListWithMany 2)
             , "test10" ~: "build from list" ~: [1] ~=? (Set.toList (Set.fromList [1,1,1]))
             , "test11" ~: "map elements" ~: Set.fromList [3,6,9] ~=? (Set.map (*3) setListWithMany)
             , "test12" ~: "map elements reduces" ~: Set.fromList [0] ~=? (Set.map (*0) setListWithMany)
             , "test13" ~: "fmap elements" ~: Set.fromList [3,6,9] ~=? (fmap (*3) setListWithMany)
             , "test14" ~: "fmap element don't reduce" ~: [0,0,0] ~=? (Set.toList (fmap (*0) setListWithMany))

             ]

run = runTestTT tests

main :: IO ()
main = do
    counts2 <- run
    if (errors counts2 + failures counts2 == 0)
        then exitSuccess
        else exitFailure
