{-
El juego nim consiste en un tablero de 5 filas numeradas de asteriscos
Tablero inicial:
1: *****
2: ****
3: ***
4: **
5: *
Dos jugadores se turnan para sacar una o mas estrellas de alguna fila
El ganador es el jugador que saca la ultima estrella
-}

main :: IO ()
main = let tablero = reverse [1..5]
       in do mostrar tablero
             putStrLn "Comienza el juego"
             nim "1" tablero

mostrar :: [Int] -> IO ()
mostrar = mostrar' 0

mostrar' :: Int -> [Int] -> IO ()
mostrar' _ [] = return ()
mostrar' n (x:xs) = let nstr = show (n+1) -- nstr :: String
                    in do putStrLn (nstr ++ ": " ++ (replicate x '*'))
                          mostrar' (n+1) xs

-- Falta chequear que fila in [1..5] y que no trate de sacar mas asteriscos de los que hay
nim :: String -> [Int] -> IO ()
nim jugador tablero = do putStrLn ("\nJugador " ++ jugador)
                         f <- fila tablero
                         a <- asteriscos f tablero
                         let tablero' = modificar tablero f a
                         mostrar tablero'
                         if tablero' == (replicate 5 0)
                            then do putStrLn ("Ha ganado el jugador " ++ jugador ++ "!")
                            else if jugador == "1"
                                    then nim "2" tablero'
                                    else nim "1" tablero'

fila :: [Int] -> IO Int
fila tablero = do putStrLn "Fila:"
                  f <- getLine
                  let f' = read f
                  if (f' < 1) || (f' > 5)
                     then do putStrLn "Fila incorrecta. Debe ser mayor que 0 y menor que 5."
                             fila tablero
                     else if tablero !! (f'-1) == 0
                             then do putStrLn "No hay asteriscos para sacar en esa fila."
                                     fila tablero
                             else return f'

-- asteriscos
-- Pide la cantidad de asteriscos a sacar.
-- Si hay suficientes en la fila, lo devuelve.
-- Si no, lo vuelve a pedir
asteriscos :: Int -> [Int] -> IO Int
asteriscos fila tablero = do putStrLn "Cantidad de asteriscos"
                             a <- getLine
                             let a' = read a
                                 x = tablero !! (fila-1)
                             if a' > x
                                then do putStrLn "No hay suficientes asteriscos en la fila mencionada"
                                        asteriscos fila tablero
                                else return a'

modificar :: [Int] -> Int -> Int -> [Int]
modificar t f a = let (xs,ys) = splitAt (f-1) t
                  in xs ++ [(head ys) - a] ++ tail ys
