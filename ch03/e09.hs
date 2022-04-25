--Define a function that joins a list of lists together using a separator value:
--intersperse :: a -> [[a]] -> [a]
--The separator should appear between elements of the list, but should not follow the last element.

intersperse :: a -> [[a]] -> [a]
intersperse e xs = case xs of
    []     -> []
    [x]    -> x
    (x:xs) -> x ++ (e : intersperse e xs)

--Your function should behave as follows.
--  ghci> :load Intersperse 
--  [1 of 1] Compiling Main        ( Intersperse.hs, interpreted )
--  Ok, modules loaded: Main. 
--  ghci> intersperse ',' [] 
--  ""
--  ghci> intersperse ',' ["foo"]
--  "foo"
--  ghci> intersperse ',' ["foo","bar","baz","quux"] 
--  "foo,bar,baz,quux"