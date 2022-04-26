import Data.Char (isSpace)
--How many of the following prelude functions can you rewrite using list folds?
--any
--cycle
--words
--unlines

--Answer:
--3, which were any, cycle, and unlines. I was forced to use "dropWhile null" after folding
--because if the input has leading spaces, the output will contain an empty string as head.

any' :: Foldable t => (a -> Bool) -> t a -> Bool
any' f xs = foldr (\x acc -> if f x then True else acc) False xs

cycle' :: [a] -> [a]
cycle' xs = foldr helper [] [1..]
  where
    helper _ acc  = xs ++ acc

words' :: String -> [String]
words' xs = tail $ foldr getWord [] xs
  where
    getWord x []  = if isSpace x then [[]] else [[x]]
    getWord x acc@(current:rest)
      | null current = if isSpace x then acc      else [x] : rest
      | otherwise    = if isSpace x then [] : acc else (x:current) : rest

unlines' :: [String] -> String
unlines' xs = foldr helper [] xs
  where
    helper aLine acc = aLine ++ ('\n':acc)

--For those functions where you can use either foldl'~ or ~foldr, which is more appropriate in each case?

--Answer:

--I used foldr for any' function because (\x acc -> if f x then True else acc) is non-strict in its 2nd argument which is the acc.

--I used foldr for the cycle function because it produces an infinite list.

--I used foldr because words function involves a lot of adding elements on a list in both Char and String level,
--therefore using the "cons" (:) operator with foldr will preserve order and become much more efficient.

--Preludes unlines function concatinates a list of strings with a '\n' in the end of each string
--I used foldr for unlines' to be able to use the "cons" (:) operator,
--Using foldl' would require the use of (++) operator in order to add a '\n' to the last string, which is less efficient than (:).
--However I was forced to use the dropwhile function 