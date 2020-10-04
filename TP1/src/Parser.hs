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
           e <- (try assgn <|> expr)
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
true = do reserved lis "true"
          return (BTrue)

false :: Parser (Exp Bool)
false = do reserved lis "false"
           return (BFalse)

bool :: Parser (Exp Bool)
bool = try (parens lis boolexp)
       <|> try true
       <|> false

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

-- Binary

andd = do reservedOp lis "&&"
          return (And)

orr = do reservedOp lis "||"
         return (Or)

-- Boolexp

boolexp :: Parser (Exp Bool)
boolexp = chainl1 (chainl1 (try unary <|> comparison) andd) orr

-----------------------------------
--- Parser de comandos
-----------------------------------

-- Skip
skipp :: Parser Comm
skipp = do reservedOp lis "skip"
           return (Skip)

-- If

iff :: Parser Comm
iff = do reserved lis "if"
         b <- boolexp
         t <- braces lis comm
         try (do reserved lis "else"
                 f <- braces lis comm
                 return (IfThenElse b t f))
             <|> return (IfThen b t)

-- While

while :: Parser Comm
while = do reserved lis "while"
           b <- boolexp
           c <- braces lis comm
           return (While b c)

-- Let

lett :: Parser Comm
lett = do v <- identifier lis
          reservedOp lis "="
          e <- intexp
          return (Let v e)

-- Seq

seqq = do reservedOp lis ";"
          return (Seq)

-- Comm

comm :: Parser Comm
comm = chainl1 (try iff <|> try while <|> try skipp <|> lett) seqq

------------------------------------
--- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
