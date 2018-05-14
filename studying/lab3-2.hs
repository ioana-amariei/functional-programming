{-
Scrieti o functie care primeste doua valori de tip Maybe Int si
intoarce o valoare de tip Maybe Int care contine catul impartirii,
daca aceasta se poate calcula, sau Nothing in caz contrar.

Aceeasi cerinta pentru functia sumaMaybe.

Extindeti tipul Exp cu constructori pentru: minus unar, minus binar,
produs, modulo, radical (intreg), exponentiere, factorial si eventual
alte operatii interesante.  Adaptati functia eval astfel incat sa
trateze si cazul noilor constructori. Testati pe cateva exemple
relevante functiile de mai sus.
-}

impartireMaybe :: Maybe Int -> Maybe Int -> Maybe Int
impartireMaybe _ (Just 0) = Nothing
impartireMaybe (Just nr1) (Just nr2) = Just (div nr1 nr2)
impartireMaybe _ _ = Nothing

sumaMaybe :: Maybe Int -> Maybe Int -> Maybe Int
sumaMaybe (Just nr) (Just 0) = Just nr
sumaMaybe (Just nr1) (Just nr2) = Just (nr1 + nr2)
sumaMaybe _ _ = Nothing

minusBinarMaybe :: Maybe Int -> Maybe Int -> Maybe Int
minusBinarMaybe (Just nr1) (Just nr2) = Just (nr1 - nr2)
minusBinarMaybe _ _ = Nothing

minusUnarMaybe :: Maybe Int -> Maybe Int
minusUnarMaybe (Just 0) = Nothing
minusUnarMaybe (Just nr) = Just (-nr)
minusUnarMaybe _ = Nothing

produsMaybe :: Maybe Int -> Maybe Int -> Maybe Int
produsMaybe _ (Just 0) = Just 0
produsMaybe (Just nr1) (Just nr2) = Just (nr1 * nr2)
produsMaybe _ _ = Nothing

moduloMaybe :: Maybe Int -> Maybe Int -> Maybe Int
moduloMaybe _ (Just 0) = Nothing
moduloMaybe _ (Just 1) = Just 0
moduloMaybe (Just nr1) (Just nr2) = Just (mod nr1 nr2)
moduloMaybe _ _ = Nothing

toInt nr = round nr

radicalMaybe :: Maybe Int -> Maybe Int
radicalMaybe (Just 0) = Just 0
radicalMaybe (Just nr) = Just (toInt (sqrt (fromIntegral nr)))
radicalMaybe _ = Nothing

exponentiereMaybe :: Maybe Int -> Maybe Int -> Maybe Int
exponentiereMaybe(Just nr1) (Just nr2) = Just (nr1 ^ nr2)
exponentiereMaybe _ _ = Nothing

factorialMaybe :: Maybe Int -> Maybe Int
factorialMaybe (Just 0) = Just 1
factorialMaybe (Just nr) = produsMaybe (Just nr) (factorialMaybe (Just (nr - 1)))
factorialMaybe _ = Nothing

{-
Mai jos avem un tip de date pentru expresii aritmetice cu numere
(intregi pe 32 de biti), "+" si "/".
-}

data Exp = Numar Int
         | Suma Exp Exp
         | MinusBinar Exp Exp
         | MinusUnar Exp
         | Produs Exp Exp
         | Impartire Exp Exp
         | Modulo Exp Exp
         | Radical Exp
         | Exponentiere Exp Exp
         | Factorial Exp
         deriving Show

{-
De exemplu, expresia matematica (3 + 4) / 5 este reprezentata de:
-- > Impartire (Suma (Numar 3) (Numar 4)) (Numar 5)
-- Impartire (Suma (Numar 3) (Numar 4)) (Numar 5)

Dandu-se o expresie aritmetica, putem calcula valoarea acesteia
folosind functia "eval":
-}


eval :: Exp -> Maybe Int
eval (Numar n) = Just n
eval (Suma e1 e2) = sumaMaybe (eval e1) (eval e2)
eval (Impartire e1 e2) = impartireMaybe (eval e1) (eval e2)
eval (MinusUnar e) = minusUnarMaybe (eval e)
eval (MinusBinar e1 e2) = minusBinarMaybe (eval e1) (eval e2)
eval (Produs e1 e2) = produsMaybe (eval e1) (eval e2)
eval (Modulo e1 e2) = moduloMaybe (eval e1) (eval e2)
eval (Radical e) = radicalMaybe (eval e)
eval (Exponentiere e1 e2) = exponentiereMaybe (eval e1) (eval e2)
eval (Factorial e) = factorialMaybe (eval e)

{-
De data aceasta, functia "eval" intoarce o valoare de tip "Maybe Int".
Daca expresia nu contine erori de tipul impartirii prin zero, atunci
rezultatul este "Just n", unde n este un Int care este valoarea
expresiei.  In caz contrar, daca expresia contine erori de tipul
impartirii prin zero, rezultatul este "Nothing".
-}

{-
Exercitiul 3

Incercati sa evaluati, folosind functia de mai sus, expresiile
aritmetice 10 / (3 + -3) si (4 + 5) / 3. Atentie! -3 este numarul
intreg -3 :: Int, nu expresia - 3, adica operatorul minus unar (pe
care nu-l avem deocamdata) aplicat numarului 3.
-}

exp1 = Impartire (Numar 10) (Suma (Numar 3) (MinusUnar (Numar 3)))
exp2 = Impartire (Suma (Numar 4) (Numar 5)) (Numar 3)
