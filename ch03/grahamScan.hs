import Data.List  (delete,sortOn)
import Data.Tuple (swap)

--Using the code from the preceding three exercises, implement Graham’s scan algorithm for the convex hull of a set of 2D points.
--You can find good description of what a convex hull is, and how the Graham scan algorithm should work, on Wikipedia.

data Direction = LEFT | RIGHT | STRAIGHT
  deriving (Show, Eq)

type Point = (Double, Double)

directionOf :: Point -> Point -> Point -> Direction
directionOf (x1, y1) (x2, y2) (x3, y3)
    | crossProduct > 0 = LEFT
    | crossProduct < 0 = RIGHT
    | otherwise        = STRAIGHT
  where
    crossProduct = (y3 - y2) * (x2 - x1) - (y2 - y1) * (x3 - x2)

bottomLeft :: [Point] -> Point
bottomLeft = swap . minimum . map swap

sortPoints :: [Point] -> [Point]
sortPoints [] = []
sortPoints ps = lowestY : sortOn polarAngle rest
  where
    polarAngle (x, y) = atan2 (y - y0) (x - x0)
    lowestY@(x0, y0)  = bottomLeft ps
    rest              = delete lowestY ps

grahamScan :: [Point] -> [Point]
grahamScan (p1:p2:p3:ps)
    | directionOf p1 p2 p3 == RIGHT = grahamScan (p1:p3:ps)
    | otherwise                     = p1 : grahamScan (p2:p3:ps)
grahamScan [p1, p2] = [p1, p2]
grahamScan _ = []

convexHull :: [Point] -> [Point]
convexHull = grahamScan . sortPoints

runExample:: [Point]
runExample = convexHull [(0, 3), (1, 1), (2, 2), (4, 4), (0, 0), (1, 2), (3, 1), (3, 3)]