module Main where

import Expressions
import Parser
import Tokenizer
import System.Environment

evalProgram :: Program -> Maybe Value
evalProgram prog = case (executeProgram empty prog) of
                      Just valuation -> valuation "result"
                      Nothing -> Nothing

interpret :: String -> IO ()
interpret program = do
  case tokenize program of
    Nothing -> putStrLn "Invalid program (error occured during tokenization phase)."
    Just tokens -> case parseProgram tokens of
        Nothing -> putStrLn "Invalid program (error occured during parsing phase)."
        Just (instructions, []) ->
            case evalProgram instructions of
              Nothing -> putStrLn "Invalid program (error occured during evaluation)."
              Just (IntValue i) -> putStrLn ("Program evaluated successfully. Result is " ++ (show i) ++ ".")
              Just (BoolValue b) -> putStrLn ("Program evaluated successfully. Result is " ++ (show b) ++ ".")
        Just (instructions, rest) ->
          putStrLn ("Could not parse this part of the program:\n" ++ (show rest))

main :: IO ()
main = do
  args <- getArgs
  case args of
    (filename:_) -> do
      contents <- readFile (head args)
      interpret contents
      _ -> putStrLn "File does not exist"
