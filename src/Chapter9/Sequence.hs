module Chapter9.Sequence where

main = do
  rs <- sequence' [getLine, getLine, getLine]
  print r
