module Chapter9.ForM where

import Control.Monad

main = do
  colors <- forM [1,2,3] (\n -> do
     putStrLn $ "Which color do you associate to " ++ show n ++ "?"
     color <- getLine
     return color)
            
  putStrLn "These are the colors you chosed:"
  mapM putStrLn colors
