import System.Environment

main = do
     args <- getArgs
     case args of
         [] -> putStrLn "Please provide an argument"
         _ -> do  contents <- readFile (head args)
                  interpret contents
