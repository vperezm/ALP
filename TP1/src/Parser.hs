module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "if", "else", "while", "skip"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        ]
    }
  )

-----------------------------------
--- Parser de expresiones enteras
-----------------------------------

-- Factor

nat :: Parser (Exp Int)
nat = do n <- natural lis
         return (Const (fromIntegral n))

var :: Parser (Exp Int)
var = do v <- identifier lis
         return (Var v)

uminus :: Parser (Exp Int)
uminus = do reservedOp lis "-"
            e <- factor
            return (UMinus e)

factor :: Parser (Exp Int)
factor = try uminus
         <|> try (parens lis intexp)
         <|> try nat
         <|> var

-- Term

times = do reservedOp lis "*"
           return (Times)

divi = do reservedOp lis "/"
          return (Div)

term :: Parser (Exp Int)
term = chainl1 factor (try times <|> divi)

-- Expr

plus = do reservedOp lis "+"
          return (Plus)

bminus = do reservedOp lis "-"
            return (Minus)

expr :: Parser (Exp Int)
expr = chainl1 term (try plus <|> bminus)

-- Extended

assgn :: Parser (Exp Int)
assgn = do v <- identifier lis
           reservedOp lis "="
           e <- expr
           return (EAssgn v e)

eseq = do reservedOp lis ","
          return (ESeq)

ext :: Parser (Exp Int)
ext = chainl1 (try assgn <|> expr) eseq

-- Intexp

intexp :: Parser (Exp Int)
intexp = ext

------------------------------------
--- Parser de expresiones booleanas
------------------------------------

-- Bool

true :: Parser (Exp Bool)
true = do reservedOp lis "true"
          return (BTrue)

false :: Parser (Exp Bool)         
false = do reservedOp lis "false"
           return (BFalse)

bool :: Parser (Exp Bool)
bool = try (parens lis boolexp)
       <|> try true
       <|> false
         
-- Binary         
         
andd = do reservedOp lis "&&"
          return (And)

orr = do reservedOp lis "||"
         return (Or)
         
binary = try andd
         <|> orr
         
-- Unary

nott :: Parser (Exp Bool)                           
nott = do reservedOp lis "!"
          e <- bool
          return (Not e)
        
unary :: Parser (Exp Bool)        
unary = try nott    
        <|> bool

-- Comparison
         
lt = do reservedOp lis "<"
        return (Lt)

gt = do reservedOp lis ">"
        return (Gt)
        
eq = do reservedOp lis "=="
        return (Eq)

neq = do reservedOp lis "!="
         return (NEq)
         
comparison :: Parser (Exp Bool)         
comparison = do m <- intexp
                e <- try lt
                     <|> try gt
                     <|> try eq
                     <|> neq
                n <- intexp
                return (e m n)
             
-- Boolexp

boolexp :: Parser (Exp Bool)
boolexp = chainl1 (try comparison <|> unary) binary

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = undefined

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
