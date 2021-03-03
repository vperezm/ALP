{-
Adivinar un numero secreto predefinido
El jugador ingresa un numero y la computadora le dice si el numero ingresado
es menor o mayor que el numero secreto o si el jugador adivino, en cuyo caso
el juego termina
-}

-- read :: String -> Int convierte una String a Int
-- show :: Int -> String

-- Función principal
main :: IO ()
main = do putStrLn "Ingrese el número secreto:"
          str <- getLine
          let secret = read str
          putStrLn "Intente adivinar el número"
          adivinar secret

adivinar :: Int -> IO ()
adivinar secret = do input <- getLine
                     let numero = read input
                     putStr "El número ingresado es"
                     if numero < secret
                        then do putStrLn " menor que el número secreto"
                                adivinar secret
                        else if numero > secret
                                then do putStrLn " mayor que el número secreto"
                                        adivinar secret
                                else do putStrLn " igual que el número secreto!"
