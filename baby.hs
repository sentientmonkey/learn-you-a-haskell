doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                      then x
                      else x*2

conanO'Brien = "It's a-me, Conan O'Brien!"

length' xs = sum [3 | _ <- xs]

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
