import Parsing
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

{-
declaration         -> type_specifier declarator ';'
declarator          -> '*' declarator
                     | direct_declarator
direct_declarator   -> direct_declarator '[' constant_expression ']'
                     | '(' direct_declarator ')'
                     | identifier
type_specifier      -> 'int' | 'char' | 'float'
constant_expression -> number
-}

{-
La producción
direct_declarator   -> direct_declarator '[' constant_expression ']'
                     | '(' direct_declarator ')'
                     | identifier
tiene recursión a izquierda.
La reemplazamos por:
direct_declarator -> '(' direct_declarator ')' direct_declarator'
                   | identifier direct_declarator'
direct_declarator' -> eps
                    | '[' constant_expression ']' direct_declarator'
-}

data CType = Type TSpec CDec deriving Show
data CDec  = Pointer CDec | Array CDec Int | Tuple CDec | Id String deriving Show
data TSpec = CInt | CChar | CFloat deriving Show

declaration :: Parser CType
declaration = do t <- token type_specifier
                 d <- token declarator
                 char ';'
                 return (Type t d)

declarator :: Parser CDec
declarator = do symbol "*"
                d <- token declarator
                return (Pointer d)
             <|> do dd <- direct_declarator
                    return dd

direct_declarator :: Parser CDec
direct_declarator = do d <- do symbol "("
                               dd <- direct_declarator
                               symbol ")"
                               return (Tuple dd)
                            <|> do i <- identifier
                                   return (Id i)
                       dd <- direct_declarator'
                       return (dd d)

direct_declarator' :: Parser (CDec -> CDec)
direct_declarator' = do symbol "["
                        c <- natural
                        symbol "]"
                        dd <- direct_declarator'
                        return (\x -> dd (Array x c))
                     <|> return id

type_specifier :: Parser TSpec
type_specifier = do t <- token (string "int")
                    return CInt
                 <|> do t <- token (string "char")
                        return CChar
                 <|> do t <- token (string "float")
                        return CFloat


eval :: String -> CType
eval xs = fst (head (parse declaration xs))
