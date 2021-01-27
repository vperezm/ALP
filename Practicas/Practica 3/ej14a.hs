data BinTree a = Leaf | Bin a (BinTree a) (BinTree a) deriving Show

foldBin :: BinTree a -> b -> (a -> b -> b -> b) -> b
foldBin    Leaf     l b = l
foldBin (Bin a t u) l b = b a (foldBin t l b) (foldBin u l b)

-- (i)
-- isLeaf determina si un arbol es una hoja
isLeaf :: BinTree a -> Bool
-- isLeaf     Leaf    = True
-- isLeaf (Bin a t u) = False
-- isLeaf tree = foldBin tree True (\a p q -> False)
isLeaf = \tree -> foldBin tree True (\a p q -> False)

-- (ii)
-- mapBin aplica la funcion argumento a todos los elementos de tipo a en el arbol
mapBin :: (a -> b) -> BinTree a -> BinTree b
-- mapBin f     Leaf    = Leaf
-- mapBin f (Bin a t u) = Bin (f a) (mapBin f t) (mapBin f u)
-- mapBin f tree = foldBin tree Leaf (\a t u -> (Bin (f a) t u))
mapBin = \f tree -> foldBin tree Leaf (\a t u -> (Bin (f a) t u))

-- (iii)
-- heightBin devuelve la altura del arbol
heightBin :: BinTree a -> Int
-- heightBin     Leaf    = 1
-- heightBin (Bin a t u) = 1 + max (heightBin t) (heightBin u)
-- heightBin tree = foldBin tree 1 (\a h1 h2 -> 1 + max h1 h2)
heightBin = \tree -> foldBin tree 1 (\a h1 h2 -> 1 + max h1 h2)

-- (iv)
-- mirrorBin devuelve el arbol espejo
mirrorBin :: BinTree a -> BinTree a
-- mirrorBin     Leaf    = Leaf
-- mirrorBin (Bin a t u) = Bin a (mirrorBin u) (mirrorBin t)
-- mirrorBin tree = foldBin tree Leaf (\a t u -> (Bin a u t))
mirrorBin = \tree -> foldBin tree Leaf (\a t u -> (Bin a u t))
