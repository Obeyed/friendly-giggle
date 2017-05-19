{-# OPTIONS_GHC -Wall #-}

import Data.List

-- Create a function that sorts a list of lists based on the length of each sublist.
-- (You may want to look at the sortBy function from the Data.List module.)
sortLists :: [[a]] -> [[a]]
sortLists xs = sortBy compareLength xs
  where compareLength a b = compare (length a) (length b)
