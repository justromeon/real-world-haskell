--Write a function lastButOne, that returns the element before the last.

lastButOne :: [a] -> a
lastButOne xs = if null (tail (tail xs)) then head xs else lastButOne (tail xs)

lastButOne' :: [a] -> a
lastButOne' (x:_:[]) = x
lastButOne' (_:y:zs)   = lastButOne' (y:zs)