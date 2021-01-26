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
mapBin = undefined

-- (iii)
-- heightBin devuelve la altura del arbol

-- (iv)
-- mirrorBin devuelve el arbol espejo
