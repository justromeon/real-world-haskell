import System.Environment (getArgs)
--Using the command framework from the earlier section “A Simple Command-Line Framework” on page 71, 
--write a program that prints the first word of each line of its input.

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
        myFunction = unlines . map (safeHead . words) . lines
          where
            safeHead aLine = if null aLine then "" else head aLine