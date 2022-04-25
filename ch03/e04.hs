--Add a type signature for your function to your source file.
--To test it, load the source file into ghci again.

len :: [a] -> Int
len []     = 0
len (_:xs) = 1 + len xs

--Or
len :: [a] -> Int
len' = foldr (\_ n -> 1 + n) 0