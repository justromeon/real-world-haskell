import Data.Char (digitToInt,isDigit)
import Data.List (foldl')

{-
Use a fold (choosing the appropriate fold will make your code much simpler) to rewrite and improve upon the asInt function from the section called “Explicit recursion”
asInt_fold :: String -> Int
    
Your function should behave as follows.

ghci> asInt_fold "101"
ghci> asInt_fold "-31337"
-31337
ghci> asInt_fold "1798"
1798
    
Extend your function to handle the following kinds of exceptional conditions by calling error.

ghci> asInt_fold ""
0
ghci> asInt_fold "-"
0
ghci> asInt_fold "-3"
-3
ghci> asInt_fold "2.7"
*** Exception: Char.digitToInt: not a digit '.'
ghci> asInt_fold "314159265358979323846"
564616105916946374
-}

asInt_fold :: String -> Int
asInt_fold "" = error "Empty String!"
asInt_fold xss@(x:xs)
    | invalidStr xs = error "Your input has decimal or contains a non-digit character"
    | overBound xss = error "Too large or too small for Int range, your input requires Integer"
    | x == '-'      = negate (foldl' step 0 xs)
    | otherwise     = foldl' step 0 xss
  where
    invalidStr str = null str || not (all isDigit str)
    overBound n    = length n >= (length $ show (maxBound :: Int))
    step acc c     = acc * 10 + digitToInt c