module Chapter9.Sequence where

main = do
  rs <- sequence [print 1, print 2, print 3]
  print rs
  print "-----"
  sequence $ map print [1,2,3]
  print "-----"
  mapM print [1,2,3]
  print "-----"
  mapM_ print [1,2,3]
