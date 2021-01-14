import Parsing
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

{-
expr   -> term ('+' expr | '-' expr | eps)
term   -> factor ('*' term | '/' term | eps)
factor -> digit | '(' expr ')'
digit  -> '0' | ... | '9'
-}

expr :: Parser Int
expr = do t <- term
          (do token (char '+')
              e <- expr
              return (t + e))
            <|> do token (char '-')
                   e <- expr
                   return (t - e)
            <|> return t

term :: Parser Int
term = do f <- factor
          (do token (char '*')
              t <- term
              return (f * t))
            <|> do token (char '/')
                   t <- term
                   return (div f t)
            <|> return f

factor :: Parser Int
factor = do d <- digit
            return (digitToInt d)
          <|> do char '('
                 e <- expr
                 char ')'
                 return e

eval :: String -> Int
eval xs = fst (head (parse expr xs))
