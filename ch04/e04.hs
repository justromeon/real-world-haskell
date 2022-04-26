import Data.List          (transpose)
import System.Environment (getArgs)

--Write a program that transposes the text in a file. For instance, it should convert ~”hello\nworld\n”~ to ~”hw\neo\nlr\nll\nod\n”~.

interactWith :: (String -> String) -> FilePath -> FilePath -> IO ()
interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main :: IO ()
main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below
        myFunction = unlines . transpose . lines