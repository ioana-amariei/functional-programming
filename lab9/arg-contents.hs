-- Scrieti o functie main care ia continutul unui fisier dat ca
-- argument la linia de comanda si afiseaza continutul acestuia
-- pe ecran.

import System.Environment
main = do
     args <- getArgs
     case args of
         [] -> putStrLn "Please provide an argument"
         _ -> do  contents <- readFile (head args)
                  putStr contents
