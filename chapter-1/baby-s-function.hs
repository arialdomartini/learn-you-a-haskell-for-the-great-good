doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

main = print (doubleUs 10 20)
