doubleMe x = x + x
doubleUs x y = doubleSmallNumber x + doubleSmallNumber y
doubleSmallNumber x = (if x > 100 then x else doubleMe x) + 1
  
main = print (doubleUs 10 200)
