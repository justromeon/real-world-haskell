--The Data.List module defines a function, groupBy, which has the following type.
--groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
    
--Use ghci to load the Data.List module and figure out what groupBy does, then write your own implementation using a fold.

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' f xs = foldr gather [] xs
  where
    gather x [] = [[x]]
    gather x acc@(receiver:rest)
        | f x (head receiver) = (x:receiver) : rest
        | otherwise           = [x] : acc