module Expressions where

data Exp =
    Number Integer
  | Boolean Bool
  | Identifier String

  | FunCall String [Exp]

  | Plus Exp Exp
  | Minus Exp Exp
  | Times Exp Exp
  | Divide Exp Exp
  | Modulo Exp Exp

  | Not Exp
  | And Exp Exp
  | Or Exp Exp

  | Equal Exp Exp
  | Greater Exp Exp
  | GreaterEq Exp Exp
  | Less Exp Exp
  | LessEq Exp Exp
  deriving Show

data Value =
    IntegerValue Integer
  | BooleanValue Bool

type ValuationPar = String -> Maybe Value

-- Exp : expresia returnata
data Function = Function [String] Block Exp

type Program = [(String, Function)]

type FuncVal = String -> Maybe Function

data Instruction =
    AssignExp String Exp
  | Cond Exp Block Block
  | Loop Exp Block
  deriving Show

type Block = [Instruction]

{- helper functions -}
liftMaybe :: (a -> b -> c) -> (a -> b -> Maybe c)
liftMaybe f x y = Just (f x y)

plusMaybe = liftMaybe (+)
minusMaybe = liftMaybe (-)
timesMaybe = liftMaybe (*)
divideMaybe = \x y -> if y == 0 then Nothing else Just (div x y)
moduloMaybe = \x y -> if y == 0 then Nothing else Just (mod x y)

liftIntegerOp :: (Integer -> Integer -> Maybe Integer) ->
  (Maybe Value -> Maybe Value -> Maybe Value)
liftIntegerOp f v1 v2 = case (v1, v2) of
                            (Just (IntegerValue x1), Just (IntegerValue x2)) -> case f x1 x2 of
                                                                                      Nothing -> Nothing
                                                                                      Just y -> Just (IntegerValue y)
                            _ -> Nothing

plusVal = liftIntegerOp plusMaybe
minusVal = liftIntegerOp minusMaybe
timesVal = liftIntegerOp timesMaybe
divideVal = liftIntegerOp divideMaybe
moduloVal = liftIntegerOp moduloMaybe

liftRelOp :: (Integer -> Integer -> Bool) ->
  (Maybe Value -> Maybe Value -> Maybe Value)
liftRelOp f v1 v2 =  case (v1, v2) of
                            (Just (IntegerValue x1), Just (IntegerValue x2)) -> Just (BooleanValue (f x1 x2))
                            _ -> Nothing

greaterVal = liftRelOp (>)
greaterEqVal = liftRelOp (>=)
lessVal = liftRelOp (<)
lessEqVal = liftRelOp (<=)
eqVal :: Maybe Value -> Maybe Value -> Maybe Value
eqVal v1 v2 = case (v1, v2) of
                    (Just (IntegerValue x1), Just (IntegerValue x2)) -> Just (BooleanValue (x1 == x2))
                    (Just (BooleanValue b1), Just (BooleanValue b2)) ->  Just (BooleanValue (b1 == b2))
                    _ -> Nothing

liftBoolOp :: (Bool -> Bool -> Bool) ->
  (Maybe Value -> Maybe Value -> Maybe Value)
liftBoolOp f v1 v2 = case (v1, v2) of
                            (Just (BooleanValue x1), Just (BooleanValue x2)) ->  Just (BooleanValue (f x1 x2))
                            _ -> Nothing

notVal :: Maybe Value -> Maybe Value
notVal (Just (BooleanValue b)) = Just (BooleanValue (not b))
notVal _ = Nothing

andVal = liftBoolOp (&&)
orVal = liftBoolOp (||)

{- evaluation functions -}
evalExpList :: [Exp] -> ValuationPar -> FuncVal -> Maybe [Value]
evalExpList [] _ _ = Just []
evalExpList (hd:tl) valp funcval = -- TODO: fill in this case

evalExp :: Exp -> ValuationPar -> FuncVal -> Maybe Value
evalExp (Number x) _ funcval = Just (IntegerValue x)
evalExp (Boolean x) _ funcval = Just (BooleanValue x)
evalExp (Identifier id) valp funcval = valp id
evalExp (FunCall id args) valp funcval = -- TODO: fill in this case
evalExp (Plus e1 e2) valp funcval = plusVal (evalExp e1 valp funcval) (evalExp e2 valp funcval)
evalExp (Minus e1 e2) valp funcval = minusVal (evalExp e1 valp funcval) (evalExp e2 valp funcval)
evalExp (Times e1 e2) valp funcval = timesVal (evalExp e1 valp funcval) (evalExp e2 valp funcval)
evalExp (Divide e1 e2) valp funcval = divideVal (evalExp e1 valp funcval) (evalExp e2 valp funcval)
evalExp (Modulo e1 e2) valp funcval = moduloVal (evalExp e1 valp funcval) (evalExp e2 valp funcval)
evalExp (Not e1) valp funcval = notVal (evalExp e1 valp funcval)
evalExp (And e1 e2) valp funcval = andVal (evalExp e1 valp funcval) (evalExp e2 valp funcval)
evalExp (Or e1 e2) valp funcval = orVal (evalExp e1 valp funcval) (evalExp e2 valp funcval)
evalExp (Greater e1 e2) valp funcval = greaterVal (evalExp e1 valp funcval) (evalExp e2 valp funcval)
evalExp (GreaterEq e1 e2) valp funcval = greaterEqVal (evalExp e1 valp funcval) (evalExp e2 valp funcval)
evalExp (Less e1 e2) valp funcval = lessEqVal (evalExp e1 valp funcval) (evalExp e2 valp funcval)
evalExp (LessEq e1 e2) valp funcval = lessEqVal (evalExp e1 valp funcval) (evalExp e2 valp funcval)
evalExp (Equal e1 e2) valp funcval = eqVal (evalExp e1 valp funcval) (evalExp e2 valp funcval)

empty :: ValuationPar
empty _ = Nothing

update :: ValuationPar -> String -> Maybe Value -> ValuationPar
update f var value = \x -> if x == var then value else f x

updateAll :: ValuationPar -> [String] -> [Value] -> Maybe ValuationPar
updateAll valp (s:rs) (v:rv) = updateAll (update valp s (Just v)) rs rv
updateAll valp [] [] = Just valp
updateAll valp _ _ = Nothing

--
executeFunction :: Function -> [Value] -> ValuationPar -> FuncVal -> Maybe Value
executeFunction (Function argNames body result) args valp funcval = case (updateAll valp argNames args) of
                                                                        Nothing -> Nothing
                                                                        Just valp' -> case executeBlock valp' body funcval of
                                                                                            Nothing -> Nothing
                                                                                            Just valp'' -> evalExp result valp'' funcval
--

emptyFunVal :: FuncVal
emptyFunVal _ = Nothing
--

updateFunVal :: FuncVal -> String -> Function -> FuncVal
updateFunVal funVal funName funCode =
  \x -> if x == funName then Just funCode else funVal x
--

executeBlock :: ValuationPar -> Block -> FuncVal -> Maybe ValuationPar
executeBlock valuation [] _ = Just valuation
executeBlock valuation (i:is) funcval = case executeInstruction valuation i funcval of
                                              Nothing -> Nothing
                                              Just valuation' -> executeBlock valuation' is funcval
--

executeInstruction :: ValuationPar -> Instruction -> FuncVal -> Maybe ValuationPar
executeInstruction valuation (AssignExp x e) funcval =
  case evalExp e valuation funcval of
        Nothing -> Nothing
        Just val -> Just (update valuation x (Just val))
executeInstruction valuation (Cond be p1 p2) funcval = -- TODO: fill in this case
executeInstruction valuation (Loop be p) funcval = case (evalExp be valuation funcval) of
                                                      Just (BooleanValue False) -> Just valuation
                                                      Just (BooleanValue True) -> case (executeBlock valuation p funcval) of
                                                                                        Nothing -> Nothing
                                                                                        Just valuation' -> executeInstruction valuation' (Loop be p) funcval
                                                      _ -> Nothing
--

startValuation :: Program -> FuncVal
startValuation [] = emptyFunVal
startValuation ((funName, fun) : rest) = updateFunVal (startValuation rest) funName fun
--

mainFunction :: Program -> Maybe Function
mainFunction [] = Nothing
mainFunction (("main", fun) : _) = Just fun
mainFunction (_:rest) = mainFunction rest
--

evalProgram :: Program -> Maybe Value
evalProgram program =
  case mainFunction program of
      Just fun -> executeFunction fun [] empty (startValuation program)
      Nothing -> Nothing
-- 
