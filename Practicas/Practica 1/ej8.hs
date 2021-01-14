import Parsing
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

{- GRAMATICA CON RECURSION A IZQUIERDA
expr   -> expr ('+' term | '-' term) | term
term   -> term ('*' factor | '/' factor) | factor
factor -> digit | '(' expr ')'
digit  -> '0' | ... | '9'
-}

{- GRAMATICA SIN RECURSION A IZQUIERDA
expr   -> term expr'
expr'  -> eps | '+' term expr' | '-' term expr'
term   -> factor term'
term'  -> eps | '*' term | '/' term
factor -> digit | '(' expr ')'
digit  -> '0' | ... | '9'
-}

expr :: Parser Int
expr = do t <- term
          g <- expr'
          return (g t)

expr' :: Parser (Int -> Int)
expr' = do char '+'
           t <- term
           g <- expr'
           return (g . (\e -> e + t))
        <|> do char '-'
               t <- term
               g <- expr'
               return (g . (\e -> e - t))
        <|> return id

term :: Parser Int
term = do f <- factor
          g <- term'
          return (g f)

term' :: Parser (Int -> Int)
term' = do char '*'
           f <- factor
           g <- term'
           return (g . (\t -> t * f))
        <|> do char '/'
               f <- factor
               g <- term'
               return (g . (\t -> div t f))
        <|> return id

factor :: Parser Int
factor = do d <- digit
            return (digitToInt d)
          <|> do char '('
                 e <- expr
                 char ')'
                 return e

eval :: String -> Int
eval xs = fst (head (parse expr xs))
