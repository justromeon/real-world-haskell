import Data.Char (digitToInt,isDigit)
import Data.List (foldl')

--The asInt_fold function uses error, so its callers cannot handle errors. Rewrite it to fix this problem.

type ErrorMessage = String

asInt_either :: String -> Either ErrorMessage Int
asInt_either "" = Left "Empty String!"
asInt_either xss@(x:xs)
    | invalidStr xs = Left "Your input has decimal or contains a non-digit character"
    | overBound xss = Left "Too large or too small for Int range, your input requires Integer"
    | x == '-'      = Right (negate $ foldl' step 0 xs)
    | otherwise     = Right (foldl' step 0 xss)
  where
    invalidStr str = null str || not (all isDigit str)
    overBound str  = length str >= (length $ show (maxBound :: Int))
    step acc c     = acc * 10 + digitToInt c