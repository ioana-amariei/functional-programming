data Figura = Triunghi Float Float Float
            | Patrat Float
            | Cerc Float
            | Poligon [(Float, Float)]
            deriving (Show)

aria :: Figura -> Float
aria (Triunghi l1 l2 l3) = l1 * l2 * l3
aria (Patrat latura) = latura * latura
aria (Cerc raza) = 2.14 * raza * raza
-- aria (Poligon (punct:puncte)) = (fst punct) + aria (Poligon puncte)

triunghi = (Triunghi 10 2 3)
patrat = (Patrat 10)
cerc = (Cerc 4)
poligon = (Poligon [(1,2), (3,2), (1,1)])
