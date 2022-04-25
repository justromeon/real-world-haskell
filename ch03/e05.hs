import Control.Monad (liftM2)
import Data.List     (genericLength)

--Write a function that computes the mean of a list, i.e. the sum of all elements in the list divided by its length.
--(You may need to use the fromIntegral function to convert the length of the list from an integer into a floating point number).

mean :: Fractional a => [a] -> a
mean xs = sum xs / (fromIntegral (length xs))

--Or
mean' :: Fractional a => [a] -> a
mean' = liftM2 (/) sum genericLength