{-# OPTIONS_GHC -Wall #-}

{- First homework http://www.seas.upenn.edu/%7Ecis194/spring13/hw/01-intro.pdf -}

-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits x
  | x < 1     = []
  | otherwise = toDigits (div x 10) ++ [mod x 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

-- Exercise 2: Double every other digit from the right
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:xs)
  | mod (length (x:y:xs)) 2 /= 0  = x : y*2 : doubleEveryOther xs
  | otherwise                     = x*2 : y : doubleEveryOther xs

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:[]) = sum (toDigits x)
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

-- Exercise 4
validate :: Integer -> Bool
validate x
  | mod (sumDigits (doubleEveryOther (toDigits x))) 10 == 0 = True
  | otherwise                                               = False


-- Exercise 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

{- Exercise 6 (optional)
 - https://en.wikipedia.org/wiki/Tower_of_Hanoi#Frame.E2.80.93Stewart_algorithm
 - http://stackoverflow.com/a/3615658/3169491
 -
 - NOTE: not correct. Not sure how to cast type properly. -}
hanoi' :: Integer -> [Peg] -> [Move]
hanoi' 0 _ = []
hanoi' n [p1, p2, p3] = hanoi n p1 p2 p3
hanoi' n (p1:p2:p3:ps) =
  hanoi' k (p1:p3:p2:ps) ++
  hanoi' (n - k) (p1:p2:ps) ++
  hanoi' k (p3:p2:p1:ps)
  where
    k = quot n 2
    --k = n - round (sqrt (2 * n + 1)) + 1 -- this keeps given an error..
hanoi' _ _ = [] -- catch all
