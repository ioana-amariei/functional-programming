{-

Am vazut cum se poate tokeniza un sir de caractere.

In continuare, vom trece la partea de parsare propriu-zisa: vom face o
functie parse care transforma un sir de tokenuri intr-o expresie
aritmetica.

Vom implementa un parser recursiv descendent foarte simplu.

-}

-- vom avea nevoie de modulul Data.Char petru diverse functii utile
import Data.Char

data Token = TInt String
           | TSuma
           | TProdus
             deriving Show

data Exp = Numar Int
         | Suma Exp Exp
         | Produs Exp Exp
         deriving Show

{-

Exercitiul 1

Pentru inceput, vom scrie un parser care va putea parsa produse de
factori, iar prin factor vom intelege pentru moment numere intregi.

Definiti functia parseFactor care primeste o lista de tokenuri si
intoarce o valoare de tip Maybe (Exp, [Token]) in felul urmator:

- Nothing daca lista de tokenuri primita ca argument nu incepe cu un
numar intreg

- Just (e, tl) daca lista de tokenuri primita ca argument incepe cu un
token de forma TInt s, e :: Exp este expresie formata doar din numarul
s iar tl sunt celelalte tokenuri (fara primul).

Exemple:

-- > parseFactor [TInt "123", TSuma, TInt "23"]
-- Just (Numar 123, [TSuma, TInt "23"])
-- > parseFactor [TSuma, TInt "23"]
-- Nothing
-- > parseFactor [TInt "23"]
-- Just (Numar 23, [])

Hint: pentru a converti un String intr-un Int, puteti folosi functia
predefinita read.

-- > read "123" :: Int
-- 123
-- > read "123" :: Float
-- 123.0
-- > :t read
-- > :i String

-}
parseFactor :: [Token] -> Maybe (Exp, [Token])

{-

Exercitiul 2

Completati definitia functiei parseTerm de mai jos.

Scopul functiei este sa parseze un termen dintr-o lista de
tokenuri. Prin termen se intelege un produs de factor.

Functia intoarce Nothing daca lista de tokenuri nu incepe cu un termen
sau Just (e, tl) daca lista de tokenuri data ca argument incepe cu
termenul e si mai contine in plus tokenurile tl.

Exemple:

-- > parseTerm [TInt "123", TProdus, TInt "23"]
-- Just (Produs (Numar 123) (Numar 23), [])
-- > parseTerm [TInt "123", TProdus, TInt "23", TProdus, TInt "12"]
-- Just (Produs (Numar 123) (Produs (Numar 23) (Numar 12)), [])
-- > parseTerm [TInt "123"]
-- Just (Numar 123, [])
-- > parseTerm [TInt "123", TProdus]
-- Just (Numar 123, [TProdus])
-- > parseTerm [TProdus]
-- Nothing

Hint: variabilele sunt denumite in felul urmator:
1) te = tokens expression
2) el = expression left
3) trest = tokens rest
4) ter = tokens expression right
5) er = expression right

Mai aveti completat cazurile cand: 1) trest este lista vida si
respectiv 2) cand trest nu este lista vida, dar incepe cu altceva
decat TProdus.

-}

parseTerm :: [Token] -> Maybe (Exp, [Token])
parseTerm te = case parseFactor te of
                 Just (el, trest) ->
                   case trest of
                     (TProdus:ter) -> case parseTerm ter of
                                       Nothing -> Just (el, TProdus:ter)
                                       Just (er, tl) -> Just ((Produs el er), tl)
                 Nothing -> Nothing

{-

Exercitiul 3.

Completati definitia functiei parseExp de mai jos.

Scopul functiei este sa parseze o expresie dintr-o lista de
tokenuri. Prin expresie se intelege o suma de termeni.

Exemple:

-- > parseExp [TInt "123", TProdus, TInt "1", TSuma, TInt "23"]
-- Just ((Suma (Produs (Numar 123) (Numar 1)) (Numar 23)), [])
-- > parseExp [TInt "123", TProdus, TInt "1"]
-- Just ((Produs (Numar 123) (Numar 1)), [])
-- > parseExp [TInt "123", TProdus, TInt "1", TProdus, TInt "2", TSuma, TInt "23"]
-- Just ((Suma (Produs (Numar 123) (Produs (Numar 1) (Numar 2))) (Numar 23)), [])
-- > parseExp [TInt "123", TProdus, TInt "1", TProdus, TInt "2", TSuma, TInt "23", TProdus, TInt "2"]
-- Just ((Suma (Produs (Numar 123) (Produs (Numar 1) (Numar 2))) (Produs (Numar 23) (Numar 2))), [])
-- > parseExp [TInt "123", TProdus, TInt "1", TProdus, TInt "2", TSuma, TInt "23", TProdus, TInt "2", TSuma, TInt "1"]
-- Just ((Suma (Produs (Numar 123) (Produs (Numar 1) (Numar 2))) (Suma (Produs (Numar 23) (Numar 2)) (Numar 1))), [])
-- > parseExp [TInt "123", TSuma, TSuma]
-- Just ((Numar 123), [TSuma, TSuma])
-- > parseExp [TInt "123"]
-- Just ((Numar 123), [])
-- > parseExp [TInt "123", TProdus, TInt "1"]
-- Just ((Produs (Numar 123) (Numar 1)), [])
-- > parseExp [TInt "123", TProdus]
-- Just ((Numar 123), [TProdus])

Hint: la fel cu parseTerm parseaza un produs de factori, la fel
parseExp trebuie sa parseze o suma de termeni, deci definitiile
functiilor parseExp si parseTerm seamana ca structura.

-}
parseExp :: [Token] -> Maybe (Exp, [Token])

{-

Exercitiul 4

In acest moment, parserul nostru poate lucra doar pe doua nivele: sume
de produse de numere intregi. Totusi, o expresie poate fi mai
complicata decat atat. De exemplu: 1+2*(4+5). In mod necesar, o astfel
de expresie contine operatorul "+" sub operatorul "*" si deci are
nevoie de paranteze.

Extindeti tipul token cu un token pentru paranteze deschise si un
token pentru paranteze inchise.

-}

{-

Exercitiul 5

In acest moment, prin definitie, un factor (in sensul functiei
parseFactor) este doar un numar intreg.

In continuare, prin factor vom intelege:
1) sau un numar intreg, ca pana acum;
2) sau o paranteza deschisa, urmata de o expresie, urmata de o paranteza inchisa.

Modificati functia parseFactor astfel incat sa parseze un factor in
sensul noii definitii de mai sus.

Exemple:

-- > parseFactor [TInt "123", TSuma, TInt "23"]
-- Just (Numar 123, [TSuma, TInt "23"])
-- > parseFactor [TSuma, TInt "23"]
-- Nothing
-- > parseFactor [TInt "23"]
-- Just (Numar 23, [])
-- > parseFactor [TParenLeft, TInt "23", TParenRight]
-- Just (Numar 23, [])
-- > parseFactor [TParenLeft, TParenLeft, TInt "23", TParenRight, TParenRight]
-- Just (Numar 23, [])
-- > parseFactor [TInt "123", TProdus, TInt "1", TSuma, TInt "23"]
-- Just ((Numar 123), [TProdus, TInt "1", TSuma, TInt "23"])

-- > parseFactor [TParenLeft, TInt "123", TProdus, TInt "1", TSuma, TInt "23", TParenRight]
-- Just ((Suma (Produs (Numar 123) (Numar 1)) (Numar 23)), [])
-- > parseFactor [TParenLeft, TInt "123", TProdus, TInt "1", TParenRight]
-- Just ((Produs (Numar 123) (Numar 1)), [])
-- > parseFactor [TParenLeft, TParenLeft, TInt "123", TProdus, TInt "1", TParenRight, TParenRight]
-- Just ((Produs (Numar 123) (Numar 1)), [])
-- > parseFactor [TInt "123", TSuma, TSuma]
-- Just ((Numar 123), [TSuma, TSuma])
-- > parseFactor [TInt "123"]
-- Just ((Numar 123), [])
-- > parseFactor [TInt "123", TProdus, TInt "1"]
-- Just ((Produs (Numar 123) (Numar 1)), [])
-- > parseFactor [TInt "123", TProdus]
-- Just ((Numar 123), [TProdus])
-- > parseFactor [TParenLeft, TInt "123", TParenRight, TProdus]
-- Just ((Numar 123), [TProdus])
-- > parseFactor [TParenLeft, TParenLeft, TInt "123", TParenRight, TProdus]
-- Nothing
-- > parseFactor [TParenLeft, TParenRight]
-- Nothing
-- > parseFactor [TParenLeft]
-- Nothing
-- > parseFactor [TParenRight]
-- Nothing
-- > parseFactor [TSuma]
-- Nothing
-- > parseFactor [TProdus]
-- Nothing

-}

{-

Exercitiu 6

In acest moment, functia parseExp apeleaza recursiv parseTerm, care
apeleaza recursiv parseFactor, care apeleaza recursiv parseExp. Avem
deci 3 functii mutual recursive care pot parsa o expresie, oricat de
mare ar fi inaltimea arborelui sintactic al expresiei.

Folositi parseExp pentru a parsa lista de tokenuri din expresiile aritmetice:
1) 3 + 4 * (2 + 6);
2) (2 + 4) * (5 + 6) + 2 * 3;
3) 1 + (2 * 3 + 4) * (2 + 4 * 5);
4) 1 + 2 + 3 * (4 + 5 * (2 + 2 * (1 + 1))).


-}

{-

Exercitiul 7

Conectati tokenizatorul din fisierul anterior cu functiile de parsare
propriu-zise din acest fisier pentru a obtine functia parse :: String
-> Maybe Exp de mai jos.

Exemple:

-- > parse "2 +  3"
-- Just (Suma (Numar 2) (Numar 3))
-- > parse "(2 + )"
-- Nothing
-- > parse "_ + 2"
-- Nothing
-- > parse "2+3*4"
-- Just (Suma (Numar 2) (Produs (Numar 3) (Numar 4)))

Hint: atentie la tokenizarea parantezelor.

-}

parse :: String -> Maybe Exp

{-

Exercitiul 8

Conectati functia eval (din al doilea fisier de azi) cu functia parse
pentru a obtine functia evalStr.

Exemple:

-- > evalStr "2+3"
-- Just 5
-- > evalStr "2*(3+4)"
-- Just 14
-- > evalStr "2++"
-- Nothing

-}
evalStr :: String -> Maybe Int
