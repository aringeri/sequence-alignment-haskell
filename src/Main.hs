module Main where
  
  import qualified SmithWaterman.NonMemoized as NonMemoized
  import qualified SmithWaterman.Memoized as Memoized

  main :: IO ()
  main = do 
            putStrLn "Enter first sequence:"
            first  <- getLine
            putStrLn "Enter second sequence:"
            second <- getLine
            putStrLn "Use memoized sequence alignment (y/n)?"
            choice <- getLine
            if invalid choice
              then main
              else putStrLn $ doAlignment choice first second

  invalid "y" = False
  invalid "n" = False
  invalid _   = True

  doAlignment "y" first second = show $ Memoized.align first second 
  doAlignment "n" first second = show $ NonMemoized.align first second
