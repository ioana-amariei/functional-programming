

{-

Exercitiul 1

Scrieti o functie care primeste doua valori de tip Maybe Int si
intoarce o valoare de tip Maybe Int care contine catul impartirii,
daca aceasta se poate calcula, sau Nothing in caz contrar.

-}

impartireMaybe :: Maybe Int -> Maybe Int -> Maybe Int
impartireMaybe _ (Just 0) = Nothing
impartireMaybe (Just n) (Just m) = Just (div n m)
impartireMaybe _ _ = Nothing


{-

Exercitiul 2

Aceeasi cerinta pentru functia sumaMaybe, minusBinarMaybe, produsMaybe, moduloMaybe, radicalMaybe.

-}

sumaMaybe :: Maybe Int -> Maybe Int -> Maybe Int
sumaMaybe (Just n) (Just m) = Just (n + m)
sumaMaybe _ _ = Nothing

minusBinarMaybe :: Maybe Int -> Maybe Int -> Maybe Int
minusBinarMaybe (Just n) (Just m) = Just (n - m)
minusBinarMaybe _ _ = Nothing

produsMaybe :: Maybe Int -> Maybe Int -> Maybe Int
produsMaybe (Just n) (Just m) = Just (n - m)
produsMaybe _ _ = Nothing

moduloMaybe :: Maybe Int -> Maybe Int -> Maybe Int
moduloMaybe _ (Just 0) = Nothing
moduloMaybe (Just n) (Just m) = Just (mod n m)
moduloMaybe _ _ = Nothing

{-

Mai jos avem un tip de date pentru expresii aritmetice cu numere
(intregi pe 32 de biti), "+" si "/".

-}

data Exp = Numar Int
         | Suma Exp Exp
         | MinusUnar Exp
	 | MinusBinar Exp Exp
	 | Produs Exp Exp
         | Impartire Exp Exp
	 | Modulo Exp Exp
         deriving Show

{-

De exemplu, expresia matematica (3 + 4) / 5 este reprezentata de:
-- > Impartire (Suma (Numar 3) (Numar 4)) (Numar 5)
-- Impartire (Suma (Numar 3) (Numar 4)) (Numar 5)

Dandu-se o expresie aritmetica, putem calcula valoarea acesteia
folosind functia "eval":

-}

minusUnarMaybe  :: Maybe Int -> Maybe Int
minusUnarMaybe (Just n) = Just (- n)
minusUnarMaybe _ = Nothing


eval :: Exp -> Maybe Int
eval (Numar n) = Just n
eval (Suma e1 e2) = sumaMaybe (eval e1) (eval e2)
eval (Impartire e1 e2) = impartireMaybe (eval e1) (eval e2)
eval (MinusUnar e) = minusUnarMaybe (eval e)
eval (MinusBinar e1 e2) = minusBinarMaybe (eval e1) (eval e2)
eval (Produs e1 e2) = produsMaybe (eval e1) (eval e2)
eval (Modulo e1 e2) = moduloMaybe (eval e1) (eval e2)

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

e1 = Impartire (Numar 10) (Suma (Numar 3) (MinusUnar (Numar 3)))
e2 = Impartire (Suma (Numar 4) (Numar 5)) (Numar 3)

{-

Exercitiul 4

Extindeti tipul Exp cu constructori pentru: minus unar, minus binar,
produs, modulo, radical (intreg), exponentiere, factorial si eventual
alte operatii interesante.  Adaptati functia eval astfel incat sa
trateze si cazul noilor constructori. Testati pe cateva exemple
relevante functiile de mai sus.

-}





radical :: Int -> Maybe Int
radical n
	| n < 0 = Nothing
	| otherwise = Just (round (sqrt (fromIntegral n)))


-- functii folosind garzi
modulo :: Int -> Int
modulo x 
      | x < 0 = -x
      | otherwise = x 


-- monad : azi am folosit monada Maybe

