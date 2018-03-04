{-
In Haskell ne putem defini propriile tipuri de date. 
Acest lucru se poate realiza utilizand cuvantul cheie `data`. 

Un exemplu simplu este tipul de date predefinit `Bool`:

  data Bool = False | True 

* `Bool` este *numele* tipului de date; 
* `False` si `True` se numesc constructori;
* `|`, care se poate citi "sau", desparte constructorii posibili 
  ai tipului de date.

Atentie, tipul `Bool` este deja predefinit in Haskell. 
Daca introducem definitia de mai sus in ghci vom crea ambiguitati!
-}


{-
Urmatoarea definitie modeleaza tipul de date Lista de numere intregi:
-}

data Lista = ListaVida | Cons Int Lista deriving (Show)

{-
Observam ca tipul de date `Lista` are doi constructori: 
* `ListaVida`  - o constanta care corespunde listei fara nici un element;
* `Cons` - care construieste o noua lista dintr-un numar intreg
  si o alta lista.

EXERCITIU: Testati in ghci care sunt tipurile constructorilor. 
EXERCITIU: Cum arata elementele de tipul Lista?
Nota:  Daca in ghci vom scrie `ListaVida`, dupa apasarea tastei `Enter`
       vom primi o eroare. Motivul este ca, pentru noul tip de date definit,
       Haskell nu stie sa-l afiseze. Pentru moment putem scapa de aceasta
       eroare decomentand `deriving (Show)` de la sfarsitul definitiei tipului.
       Vom discuta pe larg despre ceea ce face cuvantul cheie `deriving` in
       laboratoarele urmatoare. 
-}

{- 
Pana in acest moment am vazut cum putem defini tipuri noi de date. 
Dar cum scriem functii peste aceste tipuri?
Urmatoarea functie calculeaza lungimea unei liste:
-}

lungime :: Lista -> Int
lungime ListaVida = 0  -- lista vida
lungime (Cons x xs) = 1 + lungime xs  -- lista compusa

{- 
Tipul functiei `lungime` arata ca functia primeste un argument de 
tip `Lista` (tip pe care tocmai l-am definit!) si returneaza un `Int`. 
Functia este definita pentru fiecare constructor al tipului `Lista`:
* daca argumentul este `ListaVida` atunci functia returneaza 0
* daca argumentul este de forma `Cons x xs`, unde `x` si `xs` sunt
  variabile de tip Int si respectiv Lista, atunci lungimea listei este
  calculata recursiv.

Vom detalia urmatorul apel de functie: 
  `lungime (Cons 10 (Cons 43 ListaVida))`
La apelarea functiei se va activa definitia pentru lista compusa:
   `lungime (Cons x xs) = 1 + lungime xs`
Aici, sablonul (sau 'pattern'-ul)
       (Cons x xs) 
se va potrivi peste 
       (Cons 10 (Cons 43 ListaVida))
astfel
  x  <- 10
  xs <- (Cons 43 ListaVida)

Asadar, `lungime (Cons 10 (Cons 43 ListaVida))` = 1 +  lungime (Cons 43 ListaVida).
La pasul urmator, aceeasi ramura va fi activata pentru functia lungime, dar 
acum x <- 43 iar xs <- ListaVida. La final, apelul `lungime ListaVida` va returna 0. 
Executia completa a functiei este:
`lungime (Cons 10 (Cons 43 ListaVida))` = 1 +  lungime (Cons 43 ListaVida) = 
                                        = 1 + 1 + lungime ListaVida = 2 + 0 = 0.`
EXERCITIU: Testati functia de mai sus.
-}

{-
In Haskell exista o instructiune denumita `case_of` 
special pentru a face 'pattern matching'.
Aceasta constructie permite analizarea unui 
Functia de mai sus poate fi rescrisa astfel:
-}

lungime2 :: Lista -> Int
lungime2 l = case l of
                  ListaVida -> 0
                  Cons x xs -> 1 + lungime2 xs
{-
EXERCITIU: Testati functia de mai sus.
-}

{- 
EXERCITIU: Scrieti o functie care numara aparitia unui numar intreg 
           dat ca parametru intr-o lista.
-}
--numara_aparitii :: Lista -> Int -> Int
numarAparitii :: Int -> Lista -> Int
numarAparitii item ListaVida = 0;
numarAparitii item (Cons elem list) = (numarAparitii item list) + if item == elem then 1 else 0

{- 
EXERCITIU: Scrieti o functie care calculeaza suma elementelor dintr-o lista.
-}
--suma :: Lista -> Int
suma :: Lista -> Int
suma list = case list of
		ListaVida -> 0
		(Cons item item_list) -> item + suma item_list

{- 
EXERCITIU: Scrieti o functie care calculeaza maximul dintr-o lista. 
           Daca lista data ca argument este ListaVida functia va returna 0.
           Puteti folosi functii ajutatoare.
-}
--maxim :: Lista -> Int
maxim :: Lista -> Int
maxim ListaVida = 0
maxim (Cons x ListaVida) = x
maxim (Cons x xs) = max x (maxim xs)

{-
EXERCITIU: Scrieti o functie care verifica daca o lista este sortata crescator.
-}
--sortat_crescator :: Lista -> Bool
sortat_crescator :: Lista -> Bool
sortat_crescator ListaVida = True
sortat_crescator (Cons x ListaVida) = True
sortat_crescator (Cons x (Cons y xs)) = if x > y then False else sortat_crescator (Cons y xs)

{-
EXERCITIU: Scrieti o functie care concateneaza doua liste.
-}
--concatenare :: Lista -> Lista -> Lista
concatenare :: Lista -> Lista -> Lista
concatenare ListaVida list = list
concatenare (Cons x ListaVida) list = (Cons x list)
concatenare (Cons x xs) list = (Cons x (concatenare xs list))

{- 
EXERCITIU: Completati definitia tipului de data ArboreBinar de mai jos.
-}

--data ArboreBinar = ArboreVid
                -- | Nod 
                -- deriving (Show)
data ArboreBinar = ArboreVid | Nod Int ArboreBinar ArboreBinar deriving (Show)


{- 
EXERCITIU: Scrieti o functie care calculeaza inaltimea unui arbore binar.
           Puteti folosi functii ajutatoare.
-}

--inaltimeArb :: ArboreBinar -> Int 
inaltimeArb :: ArboreBinar -> Int 
inaltimeArb ArboreVid = 0
inaltimeArb 
inaltimeArb (Nod _ left right) = 1 + max (inaltimeArb left) (inaltimeArb right)


{- 
EXERCITIU: Scrieti o functie care calculeaza numarul de noduri dintr-un arbore binar.
-}

--numar_noduri :: ArboreBinar -> Int 
numar_noduri :: ArboreBinar -> Int 
numar_noduri ArboreVid = 0
numar_noduri (Nod _ left right) = 1 + (numar_noduri left) + (numar_noduri right) 

{- 
EXERCITIU: Scrieti o functie care returneaza True daca un numar se gaseste 
           intr-un nod din arbore; functia returneaza False altfel.
-}

--cauta_numar :: ArboreBinar -> Int -> Bool

{-
EXERCITIU: Scrieti o functie care parcurge arborele binar inordine.
-}
--inordine :: ArboreBinar -> Lista

{-
EXERCITIU: Scrieti o functie care verifica daca un arbore binar este arbore
           binar de cautare.
-}
--arbore_de_cautare :: ArboreBinar -> Bool
