import Parsing
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

par :: Parser a -> Parser a
par p = p <|> do char '('
                 x <- p
                 char ')'
                 return x
