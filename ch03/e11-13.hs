--e11
--Consider three two-dimensional points a, b, and c. If we look at the angle formed by the line segment from a to b and the line segment from b to c,
--it either turns left, turns right, or forms a straight line.
--Define a Direction data type that lets you represent these possibilities.

data Direction = LEFT | RIGHT | STRAIGHT
  deriving (Show, Eq)

--e12
--Write a function that calculates the turn made by three 2D points and returns a Direction.

type Point = (Double, Double)

directionOf :: Point -> Point -> Point -> Direction
directionOf (x1, y1) (x2, y2) (x3, y3)
    | crossProduct > 0 = LEFT
    | crossProduct < 0 = RIGHT
    | otherwise        = STRAIGHT
  where
    crossProduct = (y3 - y2) * (x2 - x1) - (y2 - y1) * (x3 - x2)

--e13
--Define a function that takes a list of 2D points and computes the direction of each successive triple.
--Given a list of points [a,b,c,d,e], it should begin by computing the turn made by [a,b,c], then the turn made by [b,c,d], then [c,d,e].
--Your function should return a list of Direction.

directions :: [Point] -> [Direction]
directions (p1:p2:p3:ps) = directionOf p1 p2 p3 : directions (p2:p3:ps)
directions _ = []