-- GHC - Compilator pentru Haskell
-- La fel ca programele C/C++, programele Haskell pot fi compilate.
-- Compilatorul pe care il vom folosi se numeste 'ghc' sau (varianta
-- neprescurtata) 'The Glasgow Haskell Compiler'.

-- Din nou, la fel ca in C/C++, avem o functie 'main' care serveste
-- ca "punct de intrare" in program. Mai jos avem un exemplu insotit
-- de instructiunile de compilare. Scrieti urmatoare functie intr-un
-- fisier 'helloworld.hs':

main = do putStrLn "Hello, world!"

-- Fisierul poate fi compilat astfel:

-- > ghc helloworld.hs

-- Aceasta comanda genereaza trei fisiere in directorul curent:
--  helloworld.hi, helloworld.o si helloworld (executabilul)

-- > ./helloworld
-- > Hello, world!
-- >

{-

Exercitiul 1

Verificati tipul functiei 'putStrLn'. Ce reprezinta parantezele '()'
din tipul afisat?

-}

-- putStrLn primeste ca argument un string si returneaza o *actiune* I/O.
-- Actiunile I/O au efecte secundare (de obicei pentru citire sau scriere
-- pe ecran) si contin o valoare ce trebuie returnata. In cazul afisarii
-- pe ecran acest tip este unul fara seminificatie: ().
-- Actiunile pot fi combinate folosind cuvantul cheie 'do'.


-- Pentru citirea de la tastatura vom folosi functia 'getLine'.
-- Programul urmator citeste de la tastatura un text si apoi il
-- afiseaza:

main = do
    name <- getLine
    putStrLn name

{-

Exercitiul 2

a) Compilati programul de mai sus si apoi rulati executabilul generat.
b) Verificati in ghci tipurile functiilor 'getLine', 'putStrLn' si 'main'.

-}

-- In acest moment putem sa dam o explicatie intuitiva pentru 'do':
-- acesta nu face decat sa aplice in lant actiunile declarate.
-- In cazul nostru, mai intai facem citirea si apoi scrierea.
-- Sageata '<-" are rolul de a *lega* variabila 'name' la rezultatul
-- actiunii reprezentata de functia 'getLine'.

-- Observatie: in blocul 'do' de mai sus sunt inlantuite doar actiunile I/O.
-- Daca modificam functia 'main' astfel:

-- main = do
--     name <- getLine
--     x <- 4
--     putStrLn name

-- vom primi o eroare de compilator care vine de la faptul ca 4 nu este o
-- actiune I/O.


-- Pentru citirea informatiilor din fisier, una din functiile utile
-- este 'readFile'. Programul urmator citeste continutul unui fisier
-- si il afiseaza pe ecran:

main = do
    contents <- readFile "temp.txt"
    putStr contents

{-

Exercitiul 3

Verificati tipul functiei 'readFile'.
Compilati programul de mai sus si apoi rulati executabilul generat.

-}

-- O alta functie utila este 'getArgs', care returneaza lista
-- argumentelor de la linia de comanda pentru un program.

import System.Environment
main = do
     args <- getArgs
     putStrLn (head args)

{-

Exercitiul 4

Testati programul de mai sus.

-}


{-

Exercitiul 5

Scrieti o functie main care ia continutul unui fisier dat ca
argument la linia de comanda si afiseaza continutul acestuia
pe ecran.

-}


{-

Exercitiul 6

Scrieti o functie main care ia continutul unui fisier dat ca
argument la linia de comanda si afiseaza continutul acestuia
pe ecran cu litere mari.

-}
