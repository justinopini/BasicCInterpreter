import qualified CInterperter

{-
The main function reads in the program from a file
  then returns the memory after the program runs
-} 
main = do
  putStrLn "What is your input file?"
  inName <- getLine
  contents <- readFile inName 
  let output = CInterperter.run contents
  print output 
  
