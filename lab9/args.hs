import System.Environment
main = do
     args <- getArgs
     case args of
         [] -> putStrLn "Please provide an argument"
         _ ->  putStrLn (head args)
