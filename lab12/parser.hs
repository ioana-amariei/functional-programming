module Parser where

import Tokenizer
import Expressions

data MaybeParseErr a = ParseSuccess a | ParseError Position

parseFactor :: [Token] -> MaybeParseErr (Exp, [Token])
parseTerm :: [Token] -> MaybeParseErr (Exp, [Token])
parseAExp :: [Token] -> MaybeParseErr (Exp, [Token])

parseBFactor :: [Token] -> MaybeParseErr (Exp, [Token])
parseBSignedFactor :: [Token] -> MaybeParseErr (Exp, [Token])
parseBTerm :: [Token] -> MaybeParseErr (Exp, [Token])
parseBExp :: [Token] -> MaybeParseErr (Exp, [Token])

--
parseFunCall :: String -> [Token] -> MaybeParseErr (Exp, [Token])
parseFunCall funName (TParenRight pos : rest) = ParseSuccess (FunCall funName [], rest)
parseFunCall funName l = case parseAExp l of
                              ParseSuccess (exp, (TComma _:rest)) -> case parseFunCall funName rest of
                                                                          ParseSuccess (FunCall funName args, trest) -> ParseSuccess (FunCall funName (exp:args), trest)
                                                                          ParseSuccess _ -> error "Internal parser error (parseFunCall)."
                                                                          ParseError pos -> ParseError pos
                              ParseSuccess (exp, (TParenRight _:rest)) -> ParseSuccess (FunCall funName [exp], rest)
                              ParseError pos -> ParseError pos
--

parseFactor (TInt x pos : rest) = ParseSuccess (Number (read x), rest)
parseFactor (TVar x pos : TParenLeft _ : rest) = parseFunCall x rest
parseFactor (TVar x pos : rest) = ParseSuccess (Identifier x, rest)
parseFactor (TParenLeft pos:rest) = case parseAExp rest of
                                          ParseSuccess (e, TParenRight pos:rest') -> ParseSuccess (e, rest')
                                          _ -> ParseError pos
parseFactor (token:rest) = ParseError (tokenPos token)
--

parseTerm te = case parseFactor te of
                  ParseError pos -> ParseError pos
                  ParseSuccess (el, trest) -> case trest of
                                                  (TProduct pos:ter) -> case parseTerm ter of
                                                                            ParseError _ -> ParseSuccess (el, TProduct pos:ter)
                                                                            ParseSuccess (er, tl) -> ParseSuccess ((Times el er), tl)
                                                  [] -> ParseSuccess (el, [])
                                                  l -> ParseSuccess (el, l)
--

parseAExp te = case parseTerm te of
                      ParseError pos -> ParseError pos
                      ParseSuccess (el, trest) -> case trest of
                                                        (TSum pos:ter) -> case parseAExp ter of
                                                                              ParseError _ -> ParseSuccess (el, TSum pos:ter)
                                                                              ParseSuccess (er, tl) -> ParseSuccess ((Plus el er), tl)
                                                        (TDifference pos:ter) -> case parseAExp ter of
                                                                                      ParseError _ -> ParseSuccess (el, TDifference pos:ter)
                                                                                      ParseSuccess (er, tl) -> ParseSuccess ((Minus el er), tl)
                                                        [] -> ParseSuccess (el, [])
                                                        l -> ParseSuccess (el, l)
--

parseBFactor (TTrue pos:rest) = ParseSuccess (Boolean True,  rest)
parseBFactor (TFalse pos:rest) = ParseSuccess (Boolean False, rest)
parseBFactor (TParenLeft pos:rest) = case parseBExp rest of
                                          ParseSuccess (e, TParenRight pos:rest') -> ParseSuccess (e, rest')
                                          _ -> ParseError pos
parseBFactor l = case parseAExp l of
                      ParseError pos -> ParseError pos
                      ParseSuccess (el, trest) -> case trest of
                                                        (TLess pos:rest) -> case parseAExp rest of
                                                                                  ParseSuccess (er, result) -> ParseSuccess (Less el er, result)
                                                                                  ParseError _ -> parseBFactorRest l
                                                        (TGreater pos:rest) -> case parseAExp rest of
                                                                                  ParseSuccess (er, result) -> ParseSuccess (Greater el er, result)
                                                                                  ParseError _ -> parseBFactorRest l
                                                        (TEq pos:rest) -> case parseAExp rest of
                                                                              ParseSuccess (er, result) -> ParseSuccess (Equal el er, result)
                                                                              ParseError _ -> parseBFactorRest l
                                                        (TLessEq pos:rest) -> case parseAExp rest of
                                                                                  ParseSuccess (er, result) -> ParseSuccess (LessEq el er, result)
                                                                                  ParseError _ -> parseBFactorRest l
                                                        (TGreaterEq pos:rest) -> case parseAExp rest of
                                                                                  ParseSuccess (er, result) -> ParseSuccess (GreaterEq el er, result)
                                                                                  ParseError _ -> parseBFactorRest l
                                                        _ -> parseBFactorRest l
parseBFactorRest l = case l of
                        (TVar x pos : TParenLeft _ : rest) -> parseFunCall x rest
                        (TVar x pos : rest) -> ParseSuccess (Identifier x, rest)
                        (token:rest) -> ParseError (tokenPos token)
--

parseBSignedFactor (TNot pos:trest) = case parseBFactor trest of
                                            ParseSuccess (e, trest') -> ParseSuccess (Not e, trest')
                                            _ -> ParseError pos
parseBSignedFactor trest = parseBFactor trest
--

parseBTerm te = case parseBSignedFactor te of
                      ParseError pos -> ParseError pos
                      ParseSuccess (el, trest) -> case trest of
                                                      (TAnd pos:ter) -> case parseBTerm ter of
                                                                            ParseError _ -> ParseSuccess (el, TAnd pos:ter)
                                                                            ParseSuccess (er, tl) -> ParseSuccess ((And el er), tl)
                                                      [] -> ParseSuccess (el, [])
                                                      l -> ParseSuccess (el, l)
--

parseBExp te = case parseBTerm te of
                      ParseError pos -> ParseError pos
                      ParseSuccess (el, trest) -> case trest of
                                                        (TOr pos:ter) -> case parseBExp ter of
                                                                              ParseError _ -> ParseSuccess (el, TOr pos:ter)
                                                                              ParseSuccess (er, tl) -> ParseSuccess ((Or el er), tl)
                                                        [] -> ParseSuccess (el, [])
                                                        l -> ParseSuccess (el, l)
--

parseInstruction :: [Token] -> MaybeParseErr (Instruction, [Token])
parseInstruction (TVar x pos:TAssign pos':rest) = case parseBExp rest of
                                                      ParseSuccess (e, TSemicolon pos'':rest') -> ParseSuccess (AssignExp x e, rest')
                                                      _ -> case parseAExp rest of
                                                                ParseSuccess (e, TSemicolon pos'':rest') -> ParseSuccess (AssignExp x e, rest')
                                                                _ -> ParseError pos
parseInstruction (TIf pos:rest) = case parseBExp rest of
                                      ParseSuccess (be, TThen pos' : TBraceLeft _ : restThen) -> case parseBlock restThen of
                                                                                                        ParseSuccess (pgmThen, TElse pos'' : TBraceLeft _ : restElse) -> case parseBlock restElse of
                                                                                                                                                                              ParseSuccess (pgmElse, rest') -> ParseSuccess (Cond be pgmThen pgmElse, rest')
                                                                                                                                                                              ParseError pos -> ParseError pos
                                                                                                        _ -> ParseError pos
                                      _ -> ParseError pos

parseInstruction (TWhile _ : rest) = case parseBExp rest of
                                          ParseSuccess (be, TBraceLeft _ : restWhile) -> case parseBlock restWhile of
                                                                                                ParseSuccess (body, rest') -> ParseSuccess (Loop be body, rest')
                                                                                                ParseError pos -> ParseError pos
                                          ParseError pos -> ParseError pos
parseInstruction (token:rest) = ParseError (tokenPos token)
parseInstruction [] = error "Internal error (parseInstruction)"
--

parseBlock :: [Token] -> MaybeParseErr (Block, [Token])
parseBlock (TBraceRight _ : rest) = ParseSuccess ([], rest)
parseBlock s = case parseInstruction s of
                      ParseSuccess (i, rest) -> case parseBlock rest of
                                                    ParseSuccess (is, rest') -> ParseSuccess (i:is, rest')
                                                    ParseError pos -> ParseError pos
                      ParseError pos -> ParseError pos
--

parseArgNameList :: [Token] -> MaybeParseErr ([String], [Token])
parseArgNameList (TParenRight pos : rest) = ParseSuccess ([], rest)
parseArgNameList (TVar argName pos : TParenRight _ : rest) = ParseSuccess ([argName], rest)
parseArgNameList (TVar argName pos : TComma _ : rest) = case parseArgNameList rest of
                                                            ParseSuccess (args, trest) -> ParseSuccess (argName : args, trest)
                                                            ParseError pos -> ParseError pos
parseArgNameList (token:_) = ParseError (tokenPos token)
parseArgNameList [] = error "Internal parser error (parseArgNameList)"
--

parseFunctionBody :: [Token] -> MaybeParseErr ([Instruction], Exp, [Token])
parseFunctionBody (TReturn _ : rest) = case parseAExp rest of
                                            ParseError _ -> case parseBExp rest of
                                                                ParseError pos -> ParseError pos
                                                                ParseSuccess (exp, trest) -> ParseSuccess ([], exp, trest)
                                            ParseSuccess (exp, trest) -> ParseSuccess ([], exp, trest)
parseFunctionBody l = case parseInstruction l of
                          ParseSuccess (instr, []) -> ParseError (tokenPos (head l)) -- missing return
                          ParseSuccess (instr, rest) -> case parseFunctionBody rest of
                                                            ParseSuccess (body, result, trest) -> ParseSuccess (instr : body, result, trest)
                                                            ParseError pos -> ParseError pos
                          ParseError pos -> ParseError pos
--

parseFunction :: [Token] -> MaybeParseErr ((String, Function), [Token])
parseFunction (TFunc pos : TVar funname _ : TParenLeft _ : trest) = case parseArgNameList trest of
                                                                        ParseError pos' -> ParseError pos'
                                                                        ParseSuccess (argNames, []) -> ParseError pos
                                                                        ParseSuccess (argNames, rest) -> case parseFunctionBody rest of
                                                                                                            ParseError pos -> ParseError pos
                                                                                                            ParseSuccess (body, result, rest) -> ParseSuccess ((funname, Function argNames body result), rest)
parseFunction (token:_) = ParseError (tokenPos token)
parseFunction _ = error "Internal error in parser."
--

parseProgram :: [Token] -> MaybeParseErr (Program, [Token])
parseProgram [] = ParseSuccess ([], [])
parseProgram l = case parseFunction l of
                      ParseError pos -> ParseError pos
                      ParseSuccess ((fun, funName), trest) -> case parseProgram trest of
                                                                  ParseError pos -> ParseError pos
                                                                  ParseSuccess (program, []) -> ParseSuccess ((fun, funName) : program, [])
                                                                  ParseSuccess (program, token:rest) -> ParseError (tokenPos token)
-- 
