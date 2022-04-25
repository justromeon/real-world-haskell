import Control.Arrow ((&&&))
import Data.List (sortBy)
import Data.Ord  (comparing)

--Create a function that sorts a list of lists based on the length of each sublist.
--(You may want to look at the sortBy function from the Data.List module.)

sortByLength :: [[a]] -> [[a]]
sortByLength listOfLists = sortBy compareLen listOfLists
  where
    compareLen xs ys = compare (length xs) (length ys)

--Or
sortByLength' :: [[a]] -> [[a]]
sortByLength' = sortBy (comparing length)

--Faster solution for much larger lists due to caching length
sortByLength'' :: [[a]] -> [[a]]
sortByLength'' = map fst . sortBy (comparing snd) . map (id &&& length)