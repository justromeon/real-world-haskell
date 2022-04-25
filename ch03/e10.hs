--Using the binary tree type that we defined earlier in this chapter, write a function that will determine the height of the tree.
--The height is the largest number of hops from the root to an Empty.
--For example, the tree Empty has height zero; Node "x" Empty Empty has height one; Node "x" Empty (Node "y" Empty Empty) has height two; and so on.

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

treeHeight :: Tree a -> Int
treeHeight Empty               = 0
treeHeight (Node a left right) = 1 + max (treeHeight left) (treeHeight right)