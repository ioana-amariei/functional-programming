module Parser where

import Expressions
import Tokenizer

parseFactor :: [Token] -> Maybe (AExp, [Token])
parseTerm :: [Token] -> Maybe (AExp, [Token])
parseAExp :: [Token] -> Maybe (AExp, [Token])
parseBExp :: [Token] -> Maybe (BExp, [Token])

parseTerm te = case parseFactor te of
  Nothing -> Nothing
  Just (el, trest) -> case trest of
      (TProduct:ter) -> case parseTerm ter of
        Nothing -> Just (el, TProduct:ter)
        Just (er, tl) -> Just ((Product el er), tl)
      [] -> Just (el, [])
      l -> Just (el, l)

parseAExp te = case parseTerm te of
  Nothing -> Nothing
  Just (el, trest) -> case trest of
    (TSum:ter) -> case parseAExp ter of
      Nothing -> Just (el, TSum:ter)
      Just (er, tl) -> Just ((Sum el er), tl)
    (TDifference:ter) -> case parseAExp ter of
      Nothing -> Just (el, TDifference:ter)
      Just (er, tl) -> Just ((Difference el er), tl)
    [] -> Just (el, [])
    l -> Just (el, l)

getExpBinOp :: Token -> Maybe (AExp -> AExp -> BExp)
getExpBinOp token = case token of
  TLess ->  Just Less
  TGreater ->  Just Greater
  TEq -> Just Equal
  TLessEq -> Just LessEq
  TGreaterEq -> Just GreaterEq
  _ -> Nothing

parseBExp (TTrue:rest) = Just (TrueExp,  rest)
parseBExp (TFalse:rest) = Just (FalseExp, rest)
parseBExp (TNot:rest) = case (parseBExp rest) of
                                Just(bl, rest') -> Just (Not bl, rest')
                                _ -> Nothing
parseBExp (TParenLeft:rest) = case (parseBExp rest) of
  Just (b, TParenRight:rest') -> Just (b, rest')
  _ -> Nothing
parseBExp tokens = case (parseAExp tokens) of
  Just (e, bin_op:rest') -> case getExpBinOp bin_op of
    Just bexp_op -> case (parseAExp rest') of
      Just (e', rest'') -> Just ((bexp_op e e'), rest'')
      _ -> Nothing
    _ -> case tokens of
      (TVar s:rest) -> Just (BVar s, rest)
      _ -> Nothing
  _ -> case tokens of
    (TVar s:rest) -> Just (BVar s, rest)
    _ -> Nothing

parseFactor (TInt x:rest) = Just (Number (read x), rest)
parseFactor (TVar x:rest) = Just (AVar x, rest)
parseFactor (TParenLeft:rest) = case parseAExp rest of
  Just (e, TParenRight:rest') -> Just (e, rest')
  _ -> Nothing
parseFactor _ = Nothing

parseInstruction :: [Token] -> Maybe (Instruction, [Token])
parseInstruction (TVar x:TAssignInt:rest)  = case parseAExp rest of
  Just (e, TSemicolon:rest') -> Just (AssignAExp x e, rest')
  _ -> Nothing
parseInstruction (TVar x:TAssignBool:rest)  = case parseBExp rest of
  Just (e, TSemicolon:rest') -> Just (AssignBExp x e, rest')
  _ -> Nothing
parseInstruction (TIf:rest)  = case parseBExp rest of
  Just (be, TThen:restThen) -> case parseProgram restThen of
    Just (pgmThen, TElse:restElse) -> case parseProgram restElse of
      Just (pgmElse, rest') -> Just (Cond be pgmThen pgmElse, rest')
      _ -> Nothing
    _ -> Nothing
  _ -> Nothing
parseInstruction (TWhile:rest) = case parseBExp rest of
  Just (be, restWhile) -> case parseProgram restWhile of
    Just (body, rest') -> Just (Loop be body, rest')
    _ -> Nothing
  _ -> Nothing
parseInstruction _ = Nothing

parseProgram :: [Token] -> Maybe ([Instruction], [Token])
parseProgram [] = Just ([], [])
parseProgram (TBraceLeft:s) = case parseProgram s of
  Just (pgm, TBraceRight:rest) -> Just (pgm, rest)
  _ -> Just ([], TBraceLeft:s)

parseProgram s = case parseInstruction s of
  Just (i, rest) -> case parseProgram rest of
    Just (is, rest') -> Just (i:is, rest')
    _ -> Nothing
  _ -> Just ([], s)

parse :: String -> Maybe Program
parse s = case tokenize s of
  Nothing -> Nothing
  Just tl -> case parseProgram tl of
    Just (pgm, []) -> Just pgm
    _ -> Nothing
