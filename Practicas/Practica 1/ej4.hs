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

data Expr = Num Int | BinOp Op Expr Expr deriving Show
data Op = Add | Mul | Min | Div deriving Show

expr :: Parser Expr
expr = do t <- term
          (do token (char '+')
              e <- expr
              return (BinOp Add t e))
            <|> do token (char '-')
                   e <- expr
                   return (BinOp Min t e)
            <|> return t

term :: Parser Expr
term = do f <- factor
          (do token (char '*')
              t <- term
              return (BinOp Mul f t))
            <|> do token (char '/')
                   t <- term
                   return (BinOp Div f t)
            <|> return f

factor :: Parser Expr
factor = do d <- digit
            return (Num (digitToInt d))
          <|> do char '('
                 e <- expr
                 char ')'
                 return e

eval :: String -> Expr
eval xs = fst (head (parse expr xs))
