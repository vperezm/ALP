import Parsing
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

-- [1,'a','b',2,3,'c']

data List = Empty | Cons Elem List deriving Show
data Elem = Num Int | Let Char deriving Show

brackets :: Parser List
brackets = do char '['
              xs <- list
              char ']'
              return xs

list :: Parser List
list = do x <- element
          char ','
          xs <- list
          return (Cons x xs)
       <|> do x <- element
              return (Cons x Empty)

element :: Parser Elem
element = do n <- nat
             return (Num n)
          <|> do char '\''
                 c <- letter
                 char '\''
                 return (Let c)

eval :: String -> List
eval xs = fst (head (parse brackets xs))
