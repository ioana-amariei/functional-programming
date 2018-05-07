-- module declaration
module Tokenizer where

import Data.Char

data Token = TInt String -- intregi
           | TSum -- +
           | TDifference -- -
           | TProduct -- *
           | TParenLeft -- (
           | TParenRight -- )
           | TVar String -- variabile
           | TAssignInt -- :=
           | TAssignBool -- :-
           | TSemicolon -- ;
           | TIf -- if
           | TThen -- then
           | TElse -- else
           | TWhile -- while
           | TBraceLeft -- {
           | TBraceRight -- }
           | TTrue -- true
           | TFalse -- false
           | TNot -- not
           | TLess -- <
           | TGreater -- >
           | TEq -- ==
           | TLessEq -- <=
           | TGreaterEq -- >=
             deriving Show

skipWhiteSpace :: String -> String
skipWhiteSpace (' ':rs) = skipWhiteSpace rs
skipWhiteSpace ('\n':rs) = skipWhiteSpace rs
skipWhiteSpace s = s

getInteger :: String -> (String, String)
getInteger (c:cs) | isDigit c = let (cn, cr) = getInteger cs in (c : cn, cr)
getInteger (c:cs) = ("", c:cs)
getInteger [] = ("", "")

getString  :: String -> (String, String)
getString (c:cs) | isLetter c = let (cn, cr) = getString cs in (c : cn, cr)
getString (c:cs) = ("", c:cs)
getString [] = ("", "")

keyword :: String -> Maybe Token
keyword "while" = Just TWhile
keyword "if" = Just TIf
keyword "then" = Just TThen
keyword "else" = Just TElse
keyword "true" = Just TTrue
keyword "false" = Just TFalse
keyword "not" = Just TNot
keyword _ = Nothing

tokenNext :: String -> Maybe (Token, String)
tokenNext s@(c:rest)
  | isDigit c = let (x, y) = getInteger s in Just (TInt x, y)
  | isLetter c = let (x, y) = getString  s in case keyword x of
                                                Just t -> Just (t, y)
                                                Nothing -> Just (TVar x, y)
  | c == '+' = Just (TSum, rest)
  | c == '-'  = Just (TDifference, rest)
  | c == '*' = Just (TProduct, rest)
  | c == '(' = Just (TParenLeft, rest)
  | c == ')' = Just (TParenRight, rest)
  | c == '{' = Just (TBraceLeft, rest)
  | c == '}' = Just (TBraceRight, rest)
  | c == ';' = Just (TSemicolon, rest)
  | c == ':' = case rest of
      ('=':restTail) -> Just (TAssignInt, restTail)
      ('-':restTail) -> Just (TAssignBool, restTail)
      _ -> Nothing
  | c == '=' = case rest of
                 ('=':restTail) -> Just (TEq, restTail)
                 _ -> Nothing
  | c == '<' = case rest of
                 ('=':restTail) -> Just (TLessEq, restTail)
                 _ -> Just (TLess, rest)
  | c == '>' = case rest of
                 ('=':restTail) -> Just (TGreaterEq, restTail)
                 _ -> Just (TGreater, rest)


tokenNext _      = Nothing
tokenize :: String -> Maybe [Token]
tokenize s = if skipWhiteSpace s == "" then
               Just []
             else
               case tokenNext (skipWhiteSpace s) of
                 Just (token, rest) ->
                      case tokenize rest of
                        Nothing -> Nothing
                        Just tokenList -> Just (token : tokenList)
                 Nothing -> Nothing
