{-# OPTIONS_GHC -Wall #-}

-- Write a function lastButOne, that returns the element before the last.
lastButOne :: [a] -> a
lastButOne xs = if length xs > 1
                then xs !! (length xs - 2)
                else error "Too few elements"

lastButOne' :: [a] -> Maybe a
lastButOne' xs
  | length xs > 1 = Just (lastButOne xs)
  | otherwise     = Nothing
