module Main where

import Expressions
import Parser
import Tokenizer
import System.Environment

interpret :: String -> IO ()
interpret program = do
  case tokenize (program, Position { linPos = 1, colPos = 1 }) of
    TokError pos -> putStrLn ("Error: unrecognized token at line " ++ (show (linPos pos)) ++
                              ", column " ++ (show (colPos pos)) ++ ".")
    TokSuccess tokens -> case parseProgram tokens of
        ParseError pos -> putStrLn ("Error: parsing error at line " ++ (show (linPos pos)) ++
                                    ", column " ++ (show (colPos pos)) ++ ".")
        ParseSuccess (instructions, []) ->
            case evalProgram instructions of
              Nothing -> putStrLn "Invalid program (error occured during evaluation)."
              Just (IntegerValue i) -> putStrLn ("Program evaluated successfully. Result is " ++ (show i) ++ ".")
              Just (BooleanValue b) -> putStrLn ("Program evaluated successfully. Result is " ++ (show b) ++ ".")
        ParseSuccess (instructions, token:rest) ->
          putStrLn ("Error: parsing error at line " ++ (show (linPos (tokenPos token))) ++
                                    ", column " ++ (show (colPos (tokenPos token))) ++ ".")

main :: IO ()
main = do
  args <- getArgs
  case args of
    (filename:_) -> do
      contents <- readFile (head args)
      interpret contents
    _ -> do
      putStrLn "Please provide filename."
      return ()
