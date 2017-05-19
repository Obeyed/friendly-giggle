{-# OPTIONS_GHC -Wall #-}

-- Write a function that computes the mean of a list,
-- i.e. the sum of all elements in the list divided by its length.
mean :: [Int] -> Double
mean [] = 0.0
mean xs = (/) (fromIntegral (sum xs)) (fromIntegral (length xs))


mean' :: [Int] -> Maybe Double
mean' [] = Nothing
mean' xs = Just ((/) (fromIntegral (sum xs)) (fromIntegral (length xs)))
