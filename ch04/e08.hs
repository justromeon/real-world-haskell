--Write your own definition of the standard takeWhile function, first using explicit recursion, then foldr.

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
    | f x       = x : takeWhile' f xs
    | otherwise = []

takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' f xs = foldr (\x acc -> if f x then x : acc else acc) [] xs