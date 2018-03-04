doubleMe x = x + x

doubleSmallNumber x = if x <= 100
			then doubleMe x
			else x

-- definition (or a name)
conanO'Brien = "It's a-me, Conan O'Brien!"   

-- list comprehension
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]  

nouns = ["hobo","frog","pope"]  
adjectives = ["lazy","grouchy","scheming"]  

length' list = sum [ 1 | _ <- list ]

removeNonUppercase list = [ item | item <- list , item `elem` ['A'..'Z'] ]
 

-- definire tip Lista
data Lista = ListaVida | Cons Int Lista deriving (Show)

-- lungime lista
lungime :: Lista -> Int
lungime ListaVida = 0
lungime ( Cons item item_list ) = 1 + lungime item_list


-- lungime lista "case of"
lungime2 :: Lista -> Int
lungime2 list = case list of
                  ListaVida -> 0
                  Cons item list -> 1 + lungime2 list


-- numarul aparitiilor unui element intr-o lista
numarAparitii :: Int -> Lista -> Int
numarAparitii item ListaVida = 0;
numarAparitii item (Cons elem list) = (numarAparitii item list) + if item == elem then 1 else 0


-- suma elementelor intr-o lista
suma :: Lista -> Int
suma list = case list of
		ListaVida -> 0
		(Cons item item_list) -> item + suma item_list
	

-- maximul dintr-o lista
list = (Cons 3 (Cons 2 (Cons 3 ListaVida)))

maxim :: Lista -> Int
maxim ListaVida = 0
maxim (Cons x ListaVida) = x
maxim (Cons x xs) = max x (maxim xs)


-- verifica daca o functie este sortata crescator
sortat_crescator :: Lista -> Bool
sortat_crescator ListaVida = True
sortat_crescator (Cons x ListaVida) = True
sortat_crescator (Cons x (Cons y xs)) = if x > y then False else sortat_crescator (Cons y xs)


l1 = (Cons 1 (Cons 2 ListaVida))
l2 = (Cons 2 (Cons 3 (Cons 5 ListaVida)))
l3 = (Cons 3 (Cons 4 ListaVida))

-- concatenarea a doua liste
concatenare :: Lista -> Lista -> Lista
concatenare ListaVida list = list
concatenare (Cons x ListaVida) list = (Cons x list)
concatenare (Cons x xs) list = (Cons x (concatenare xs list))


data ArboreBinar = ArboreVid | Nod Int ArboreBinar ArboreBinar deriving (Show)

inaltimeArb :: ArboreBinar -> Int 
inaltimeArb ArboreVid = 0
inaltimeArb (Nod _ left right) = 1 + max (inaltimeArb left) (inaltimeArb right)

numar_noduri :: ArboreBinar -> Int 
numar_noduri ArboreVid = 0
numar_noduri (Nod _ left right) = 1 + (numar_noduri left) + (numar_noduri right) 

arb = (Nod 2 (Nod 3 ArboreVid ArboreVid) (Nod 5 ArboreVid ArboreVid))
arbCautare = (Nod 5 (Nod 3 ArboreVid ArboreVid) (Nod 7 (Nod 6 ArboreVid ArboreVid) ArboreVid))

cauta_numar :: ArboreBinar -> Int -> Bool
cauta_numar ArboreVid _ = False
cauta_numar (Nod info left right) nr = if nr == info then True 
                                       else (cauta_numar left nr) || 						   (cauta_numar right nr)

inordine :: ArboreBinar -> Lista
inordine ArboreVid = ListaVida
inordine (Nod info ArboreVid ArboreVid) = (Cons info ListaVida)
inordine (Nod info left ArboreVid) = concatenare (inordine left) (Cons info ListaVida)
inordine (Nod info ArboreVid right) = (Cons info (inordine right))
inordine (Nod info left right) =  concatenare (inordine left) (Cons info (inordine right))


arbore_de_cautare :: ArboreBinar -> Bool
arbore_de_cautare ArboreVid = True
arbore_de_cautare (Nod _ ArboreVid ArboreVid) = True
arbore_de_cautare (Nod infoParinte (Nod infoStang left right) ArboreVid) = if infoParinte >= infoStang then arbore_de_cautare (Nod infoStang left right) else False
arbore_de_cautare (Nod infoParinte ArboreVid (Nod infoDrept left right)) = if infoParinte < infoDrept then arbore_de_cautare (Nod infoDrept left right) else False
arbore_de_cautare (Nod infoParinte (Nod infoStang left1 right1) (Nod infoDrept left2 right2)) = 
 if (infoParinte < infoStang) then False else (if infoParinte >= infoDrept then False else (arbore_de_cautare (Nod infoStang left1 right1)) || (arbore_de_cautare (Nod infoDrept left2 right2)))




data Point = (Int, Int)

addPoint Point -> Point -> Point
addPoint (x1,y1) (x2,y2) = ((x1 + x2) , (y1 + y2))






