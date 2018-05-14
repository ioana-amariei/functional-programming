module Tokenizer where

import Data.Char

data MaybeTokErr a = TokSuccess a | TokError Position

-- record (sugar syntax pentru declaratiile de functii si tipurile lor)
-- equiv: data Position = Position Int intregi
--  ma pot referi la Int ca linPos, respectiv colPos (care sunt functii)
data Position = Position { linPos :: Int, colPos :: Int } deriving Show

data Token = TInt String Position -- intregi
           | TSum Position -- +
           | TDifference Position -- -
           | TProduct Position -- *
           | TParenLeft Position -- (
           | TParenRight Position -- )
           | TVar String Position -- variabile
           | TAssign Position -- :=
           | TSemicolon Position -- ;
           | TIf Position -- if
           | TThen Position -- then
           | TElse Position -- else
           | TWhile Position -- while
           | TBraceLeft Position -- {
           | TBraceRight Position -- }
           | TTrue Position -- true
           | TFalse Position -- false
           | TNot Position -- not
           | TAnd Position -- and
           | TOr Position -- not
           | TLess Position -- <
           | TGreater Position -- >
           | TEq Position -- ==
           | TLessEq Position -- <=
           | TGreaterEq Position -- >=
           | TComma Position -- ,
           | TReturn Position -- return
           | TFunc Position -- func
           deriving Show

tokenPos :: Token -> Position
tokenPos (TInt _ pos) = pos
tokenPos (TSum pos) = pos
tokenPos (TDifference pos) = pos
tokenPos (TProduct pos) = pos
tokenPos (TParenLeft pos) = pos
tokenPos (TParenRight pos) = pos
tokenPos (TVar _ pos) = pos
tokenPos (TAssign pos) = pos
tokenPos (TSemicolon pos) = pos
tokenPos (TIf pos) = pos
tokenPos (TThen pos) = pos
tokenPos (TElse pos) = pos
tokenPos (TWhile pos) = pos
tokenPos (TBraceLeft pos) = pos
tokenPos (TBraceRight pos) = pos
tokenPos (TTrue pos) = pos
tokenPos (TFalse pos) = pos
tokenPos (TNot pos) = pos
tokenPos (TAnd pos) = pos
tokenPos (TOr pos) = pos
tokenPos (TLess pos) = pos
tokenPos (TGreater pos) = pos
tokenPos (TEq pos) = pos
tokenPos (TLessEq pos) = pos
tokenPos (TGreaterEq pos) = pos

type PointedString = (String, Position)

nextCol :: Position -> Position
nextCol (Position { linPos = x, colPos = y }) = (Position {linPos = x, colPos = y + 1})

nextLin :: Position -> Position
nextLin (Position { linPos = x, colPos = y }) = (Position {linPos = x + 1, colPos = 1})

skipWhiteSpace :: PointedString -> PointedString
skipWhiteSpace ((' ':rs), pos) = skipWhiteSpace (rs, nextCol pos)
skipWhiteSpace (('\n':rs), pos) = skipWhiteSpace (rs, nextLin pos)
skipWhiteSpace ps = ps

getInteger :: PointedString -> (String, PointedString)
getInteger ((c:cs), pos) | isDigit c = let (cn, ps) = getInteger (cs, nextCol pos) in (c : cn, ps)
getInteger ps = ("", ps)

getId  :: PointedString -> (String, PointedString)
getId ((c:cs), pos) | isLetter c = let (cn, ps) = getId (cs, nextCol pos) in (c : cn, ps)
getId ps = ("", ps)

keyword :: String -> Maybe (Position -> Token)
keyword "func" = Just TFunc
keyword "return" = Just TReturn
keyword "while" = Just TWhile
keyword "and" = Just TAnd
keyword "or" = Just TOr
keyword "if" = Just TIf
keyword "then" = Just TThen
keyword "else" = Just TElse
keyword "true" = Just TTrue
keyword "false" = Just TFalse
keyword "not" = Just TNot
keyword _ = Nothing

tokenNext :: PointedString -> MaybeTokErr (Token, PointedString)
tokenNext ps@(s@(c:rs), pos)
  | isDigit c = let (x, y) = getInteger ps in TokSuccess (TInt x pos, y)
  | isLetter c = let (x, y) = getId  ps in case keyword x of
                                                Just t -> TokSuccess (t pos, y)
                                                Nothing -> TokSuccess (TVar x pos, y)
  | c == ',' = TokSuccess (TComma pos, (rs, nextCol pos))
  | c == '+' = TokSuccess (TSum pos, (rs, nextCol pos))
  | c == '-'  = TokSuccess (TDifference pos, (rs, nextCol pos))
  | c == '*' = TokSuccess (TProduct pos, (rs, nextCol pos))
  | c == '(' = TokSuccess (TParenLeft pos, (rs, nextCol pos))
  | c == ')' = TokSuccess (TParenRight pos, (rs, nextCol pos))
  | c == '{' = TokSuccess (TBraceLeft pos, (rs, nextCol pos))
  | c == '}' = TokSuccess (TBraceRight pos, (rs, nextCol pos))
  | c == ';' = TokSuccess (TSemicolon pos, (rs, nextCol pos))
  | c == ':' = case rs of
                  ('=':rstail) -> TokSuccess (TAssign pos, (rstail, nextCol (nextCol pos)))
                  _ -> TokError pos
  | c == '=' = -- TODO: fill in this case
  | c == '<' = case rs of
                 ('=':rstail) -> TokSuccess (TLessEq pos, (rstail, nextCol (nextCol pos)))
                 _ -> TokSuccess (TLess pos, (rs, nextCol pos))
  | c == '>' = case rs of
                 ('=':rstail) -> TokSuccess (TGreaterEq pos, (rstail, nextCol (nextCol pos)))
                 _ -> TokSuccess (TGreater pos, (rs, nextCol pos))
  | otherwise = TokError pos

tokenize :: PointedString -> MaybeTokErr [Token]
tokenize s = case skipWhiteSpace s of
                   ("", _) -> TokSuccess []
                   ss -> case tokenNext ss of
                             TokSuccess (token, rest) -> case tokenize rest of
                                                                TokError e -> TokError e
                                                                TokSuccess tokenList -> TokSuccess (token : tokenList)
                             TokError e -> TokError e
