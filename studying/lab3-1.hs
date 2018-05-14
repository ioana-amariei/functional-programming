{-

Mai jos avem un tip de date pentru expresii aritmetice cu numere
(intregi pe 32 de biti), + si *.

De exemplu, expresia matematica (3 + 4) * 5 este reprezentata de
valoarea Haskell:

-- Produs (Suma (Numar 3) (Numar 4)) (Numar 5)

-}

data Exp = Numar Int
         | Suma Exp Exp
         | Produs Exp Exp
         | MinusUnar Exp
         | MinusBinar Exp Exp
         | Impartire Exp Exp
         deriving Show

{-
Exercitiul 1

Cum reprezentam expresia aritmetica 4 * (3 + 2) * 2?
-}

exp0 = Produs (Produs (Numar 4) (Suma (Numar 3) (Numar 2))) (Numar 2)

{-
Dandu-se o expresie aritmetica, putem calcula valoarea acesteia
folosind functia "eval":
-}

-- eval :: Exp -> Int
-- eval (Numar n) = n
-- eval (Suma e1 e2) = (eval e1) + (eval e2)
-- eval (MinusBinar e1 e2) = (eval e1) - (eval e2)
-- eval (Produs e1 e2) = (eval e1) * (eval e2)
-- eval (Impartire e1 e2) = div (eval e1) (eval e2)
-- eval (MinusUnar e) = - (eval e)

exp1 = Produs (Produs (Numar 4) (Suma (Numar 3) (Numar 2))) (Numar 2)
exp2 = Suma (Produs (Numar 4) (Suma (Numar 3) (Numar 2))) (Numar 2)

{-
Exercitiul 2

Extindeti tipul Exp cu constructori pentru:

1) minus unar (e.g. MinusUnar (Numar 5) reprezinta expresia "-5")
2) minus binar (e.g. MinusBinar (Numar 3) (Numar 3) reprezinta expresia "-3")

Adaptati functia eval astfel incat sa trateze si cazul noilor
constructori.
Evaluati folosind functia voastra expresiile -(3 + 4) si
((-3) * 4) + (10 * 2).
-}

exp3 = MinusUnar (Suma (Numar 3) (Numar 4))
exp4 = Suma (Produs (MinusUnar (Numar 3)) (Numar 4)) (Produs (Numar 10) (Numar 2))

{-
Exercitiul 3

Extindeti expresiile cu operatia de impartire.
Ce se intampla in cazul unei impartiri prin 0?
-}

exp5 = Impartire (Suma (Numar 2) (Numar 4)) (Numar 0)
-- *** Exception: divide by zero

{-
Tipuri parametrizate

Tipul parametrizat List:
> import Data.List
> :i []
data [] a = [] | a : [a]
...
In definitia tipul de date List, "a" este o variabila de tip.
Prin urmare definitia listelor este parametrica in tipul elementelor.
...

Tipul parametrizat Maybe:

> :i Maybe
data Maybe a = Nothing | Just a
...
Tipul "Maybe a" contine toate valorile tipului "a", protejate de constructorul "Just",
precum si inca o valoare "Nothing". De exemplu, valorile de tip Maybe Int sunt
Nothing, Just 0, Just (-2), Just 7, ...

Cum ne-ar putea ajuta acest tip sa tratam cazul impartirii la zero?
-}

-- data Maybe a = Nothing | Just a

eval :: Exp -> Maybe Int

eval (Numar n) = Just n

eval (Suma e1 e2) = case ((eval e1), (eval e2)) of
                            (Just nr, Just 0) -> Just nr
                            (Just nr1, Just nr2) -> Just (nr1 + nr2)
                            (_, _) -> Nothing

eval (MinusBinar e1 e2) = case ((eval e1), (eval e2)) of
                            (Just nr1, Just nr2) -> Just (nr1 - nr2)
                            (_, _) -> Nothing

eval (Produs e1 e2) = case ((eval e1), (eval e2)) of
                            (Just nr, Just 0) -> Just 0
                            (Just nr1, Just nr2) -> Just (nr1 * nr2)
                            (_, _) -> Nothing

eval (Impartire e1 e2) = case ((eval e1), (eval e2)) of
                            (_, Just 0) -> Nothing
                            (Just nr1, Just nr2) -> Just (div nr1 nr2)
                            (_, _) -> Nothing

eval (MinusUnar e) = case (eval e) of
                        (Just 0) -> Nothing
                        (Just nr) -> Just (- nr)
                         -- _  -> Nothing
