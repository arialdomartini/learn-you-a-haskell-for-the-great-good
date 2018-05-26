doubleMe x = x + x
doubleUs x y = doubleSmallNumber x + doubleSmallNumber y
doubleSmallNumber x =
  if x > 100 then x else doubleMe x
  
main = print (doubleUs 10 200)
