-- tipul pentru expresii aritmetice definit in laboratoarele
-- precedente

data Exp = Var String
         | Numar Int
         | Suma Exp Exp
         | Diferenta Exp Exp
         | Produs Exp Exp deriving Show

-- tipul pentru o valuatie partiala

type ValuationPar = String -> Maybe Int

-- functii ajutatoare din laboratorul precedent

lift2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
lift2 f Nothing _ = Nothing
lift2 f _ Nothing = Nothing
lift2 f (Just x) (Just y) = Just (f x y)

sumaMaybe :: Maybe Int -> Maybe Int -> Maybe Int
sumaMaybe = lift2 (+)

diferentaMaybe :: Maybe Int -> Maybe Int -> Maybe Int
diferentaMaybe = lift2 (-)

produsMaybe :: Maybe Int -> Maybe Int -> Maybe Int
produsMaybe = lift2 (*)

-- functia de evaluare care asociaza unei expresii si unei valuatii
-- partiale o valoare de tip Maybe Int reprezentand fie rezultatul
-- evaluarii (sub forma Just x), fie faptul ca in timpul evaluarii a
-- avut loc o eroare (Nothing).

evalp :: Exp -> ValuationPar -> Maybe Int
evalp (Numar x) _ = Just x
evalp (Var y) valp = valp y
evalp (Suma e1 e2) valp = sumaMaybe (evalp e1 valp) (evalp e2 valp)
evalp (Diferenta e1 e2) valp = diferentaMaybe (evalp e1 valp) (evalp e2 valp)
evalp (Produs e1 e2) valp = produsMaybe (evalp e1 valp) (evalp e2 valp)

-- valuatia "vida" din laboratorul precedent
empty :: ValuationPar
empty _ = Nothing

-- functia de actualizare a unei valuatii din laboratorul procedent
update :: ValuationPar -> String -> Maybe Int -> ValuationPar
update f variabila valoare = \x -> if x == variabila then valoare else f x

-- tipul de date pentru un program
type Program = [Instr]

-- functia de executie a unui program din laboratorul precedent
executaProgram :: ValuationPar -> Program -> Maybe ValuationPar
executaProgram valuatie [] = Just valuatie
executaProgram valuatie (instructiune:instructiuni) = case executaInstr valuatie instructiune of
                                                             Nothing -> Nothing
                                                             Just valuatie' -> executaProgram valuatie' instructiuni

-- Pana in acest moment programele contin *doar* atribuiri.
-- Mai departe, vom simula instructiunile conditionale.
-- Completati tipul de date de mai jos cu instructiuni
-- conditionale cu doua ramuri.
-- Conditiile pot fi reprezentate utilizand expresiile deja definite
-- (i.e., tipul Exp).
data Instr = Atrib String Exp
          | Cond Exp Program Program
          | Bucla Exp Program
          | For Instr Exp Instr Program

{-
Exercitiul 1

Completati functia de executie a unei instructiuni cu cazul pentru
instructiunile conditionale. Deoarece conditiile sunt reprezentate
folosind expresii, vom da instructiunilor conditionale urmatoarea
semantica: cand expresia este evaluata la o valoarea diferita de
0 atunci vom executa programul corespunzator primei ramuri;
complementar, cand expresia este evaluata la 0 atunci vom
executa programul corespunzator celei de-a doua ramuri.
-}

executaInstr :: ValuationPar -> Instr -> Maybe ValuationPar
executaInstr valuatie (Atrib x expresie) = Just (update valuatie x (evalp expresie valuatie))
executaInstr valuatie (Cond expresie p1 p2) = case (evalp expresie valuatie) of
                                                    Nothing   -> Nothing
                                                    Just 0    -> executaProgram valuatie p2
                                                    otherwise -> executaProgram valuatie p2
executaInstr valuatie (Bucla expresie program) = case (evalp expresie valuatie) of
                                                        Nothing   -> Nothing
                                                        Just 0    -> Just valuatie
                                                        otherwise -> case (executaProgram valuatie program) of
                															Nothing -> Nothing
                															Just valuatie' -> executaInstr valuatie' (Bucla expresie program)
executaInstr valuatie (For instr1 expresie instr2 program) = case (executaInstr valuatie instr1) of
                                                                    Nothing -> Nothing
                                                                    Just valuatie' -> executaInstr valuatie' (Bucla expresie (program ++ [instr2]))


-- functia de evaluare a unui program
evalProgram :: Program -> Maybe Int
evalProgram prog = case (executaProgram empty prog) of
                      Just valuatie -> valuatie "result"
                      Nothing       -> Nothing

{-
Exercitiul 2

Testati functia de evaluare a unui program pe programul prog1 de mai
jos.
-}

prog1 :: Program
prog1 = [Atrib "n" (Numar 10),
         Atrib "s" (Numar 0),
         Cond (Var "n") [Atrib "x" (Numar 1), Atrib "y" (Numar 3)]
                        [Atrib "x" (Numar 2), Atrib "y" (Numar 4)],
         Atrib "result" (Suma (Var "x") (Var "y"))]

{-
Execitiul 3

Completati tipul de date pentru instructiuni cu o instructiune pentru
bucle (de tip "while").

Completati functia de executie a unei instructiuni pentru a trata
bucla while.

Testati functia de evaluare pentru programul de mai jos.
-}

prog2 :: Program
prog2 = [Atrib "n" (Numar 10),
        Atrib "s" (Numar 0),
        Bucla (Var "n") [Atrib "s" (Suma (Var "s") (Var "n")),
                         Atrib "n" (Diferenta (Var "n") (Numar 1))
                         ],
        For (Atrib "i" (Numar 10)) (Var "i") (Atrib "i" (Diferenta (Var "i") (Numar 1))) [Atrib "s" (Suma (Var "s") (Var "i"))],
        Atrib "result" (Var "s")]

{-
Exercitiul 4

Adagati instructiunea "for"; nu uitati sa testati pe cateva exemple.
-}


{-
Exercitiul 5 (optional)

Extindeti parserul pentru a parsa instructiuni si secvente de
instructiuni.

Conectati noul parser cu functia de evaluare.
-}
