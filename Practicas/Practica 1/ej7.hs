import Parsing
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

data Hasktype = DInt | DChar | Fun Hasktype Hasktype deriving Show

-- Int -> Char -> Int
-- Fun DInt (Fun DChar DInt)

hasktype :: Parser Hasktype
hasktype = do t <- singletype
              (do symbol "->"
                  ts <- hasktype
                  return (Fun t ts)
                <|> return t)

singletype :: Parser Hasktype
singletype = do token (string "Int")
                return DInt
             <|> do token (string "Char")
                    return DChar

eval :: String -> Hasktype
eval xs = fst (head (parse hasktype xs))
