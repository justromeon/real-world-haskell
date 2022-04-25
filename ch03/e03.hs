--Write a function that computes the number of elements in a list.
--To test it, ensure that it gives the same answers as the standard length function.

len []     = 0
len (_:xs) = 1 + len xs

--Or
len' = foldr (\_ n -> 1 + n) 0