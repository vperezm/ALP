import Parsing
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

data Basetype = DInt | DChar | DFloat deriving Show
type Hasktype = [Basetype]

-- Int -> Char -> Float
-- [DInt, DChar, DFloat] :: Hasktype

hasktype :: Parser Hasktype
hasktype = sepBy1 (token onetype) (symbol "->")

onetype :: Parser Basetype
onetype = do string "Int"
             return DInt
          <|> do string "Char"
                 return DChar
          <|> do string "Float"
                 return DFloat

eval :: String -> Hasktype
eval xs = fst (head (parse hasktype xs))
