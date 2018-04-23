-- Scrieti o functie main care ia continutul unui fisier dat ca
-- argument la linia de comanda si afiseaza continutul acestuia
-- pe ecran cu litere mari.

import System.Environment
import Data.Char

uppercase :: String -> String
uppercase = map toUpper

main = do
     args <- getArgs
     case args of
         [] -> putStrLn "Please provide an argument"
         _ -> do  contents <- readFile (head args)
                  putStr (uppercase contents)
                  -- putStr (map toUpper contents)
