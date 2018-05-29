lostNumbers = [4,8,15,16,23,42]

main =
  print "Run individual functions, please"

printList = print lostNumbers

concatenate = lostNumbers ++ [9,10,11,12]
cons = 0 : [1,2,3,4]

addLast = [1,2,3,4] ++ [5]

-- this is not valid
-- concatenate = [1,2,3,4] ++ 5

accessingItem list position = list !! position
access = accessingItem [1,2,15,18,0] 2 -- should be 15

accessRanges = tail (take 4 ['a'..'z']) -- should be ['b','c','d']

listComprehension1 = [2 * x | x <- [1..10]]
listComprehension2 = [2 * x | x <- [1..10], 2 * x >= 12]
listComprehension3 = [x | x <- [50..100], x `mod` 7 == 3]

listComprehension4 = [ if x < 10 then "bang" else "boom" | x <- [1..100], odd x]
boomBangs xs =       [ if x < 10 then "bang" else "boom" | x <- xs, odd x]
listComprehension5 = boomBangs [1..100]

multiplePredicates = [x + y | x <- [1,2,3], y <-[1..100]]

productOfAllPossibleCombinations xs ys = [x * y | x <- xs, y <- ys]
tryProduct = productOfAllPossibleCombinations [1,2,3] [4,5,6]


docker_container_names = [adjective ++ "_" ++ noun |
                          adjective <- ["lazy", "boring", "amazing", "cool"],
                          noun <- ["einstein", "turing", "john"] ]

length' xs = sum [1 | _ <- xs]


removeUpperCase s = [c | c <- s, not(c `elem` ['A'..'Z'])]
tryRemoveUpperCase = removeUpperCase "ciao mamImMOaL MAgIuAarBda LAcAoOme mi diverto!"
