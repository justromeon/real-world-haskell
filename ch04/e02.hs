--Write a function splitWith that acts similarly to words, but takes a predicate and a list of any type,
--and splits its input list on every element for which the predicate returns False.

splitWith :: (a -> Bool) -> [a] -> [[a]]
slitWith _ [] = []
splitWith f xs
    | null aList = splitWith f rest'
    | otherwise  = aList : splitWith f rest'
  where
    (aList, rest) = span f xs
    rest'         = dropWhile (not . f) rest