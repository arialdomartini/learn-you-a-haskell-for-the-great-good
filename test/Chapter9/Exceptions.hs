module Chapter9.Exceptions where

div' :: (Integral a) => a -> a -> Maybe a
div' _ 0 = Nothing
div' a b = Just (a `div` b)
