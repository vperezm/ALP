{
module Parse where
import Common
import Data.Maybe
import Data.Char

}

%monad { P } { thenP } { returnP }
%name parseStmt Def
%name parseStmts Defs
%name term Exp

%tokentype { Token }
%lexer {lexer} {TEOF}

%token
    '='     { TEquals }
    ':'     { TColon }
    '\\'    { TAbs }
    '.'     { TDot }
    '('     { TOpen }
    ')'     { TClose }
    '->'    { TArrow }
    ','     { TComma }
    VAR     { TVar $$ }
    TYPEE   { TTypeE }
    TYPEU   { TTypeU }
    TYPEN   { TTypeN }
    DEF     { TDef }
    LET     { TLet }
    IN      { TIn }
    AS      { TAs }
    UNIT    { TUnit }
    FST     { TFst }
    SND     { TSnd }
    ZERO    { TZero }
    SUC     { TSuc }
    REC     { TRec }


%right VAR
%left '='
%right '->'
%right '\\' '.' LET
%nonassoc FST SND
%nonassoc SUC
%left REC
%left AS

%%

Def     :  Defexp                      { $1 }
        |  Exp	                       { Eval $1 }
Defexp  : DEF VAR '=' Exp              { Def $2 $4 }

Exp     :: { LamTerm }
        : '\\' VAR ':' Type '.' Exp    { LAbs $2 $4 $6 }
        | LET VAR '=' Exp IN Exp       { LLet $2 $4 $6 }
        | As                           { $1 }

As      :: { LamTerm }
        : Exp AS Type                  { LAs $1 $3 }
        | Rec                          { $1 }

Rec     :: { LamTerm }
        : REC Atom Atom Exp            { LRec $2 $3 $4 }
        | Suc                          { $1 }

Suc     :: { LamTerm }
        : SUC Exp                      { LSuc $2 }
        | FS                           { $1 }

FS      :: { LamTerm }
        : FST Exp                      { LFst $2 }
        | SND Exp                      { LSnd $2 }
        | NAbs                         { $1 }

NAbs    :: { LamTerm }
        : NAbs Atom                    { LApp $1 $2 }
        | Atom                         { $1 }

Atom    :: { LamTerm }
        : VAR                          { LVar $1 }
        | UNIT                         { LUnit }
        | ZERO                         { LZero }
        | '(' Exp ')'                  { $2 }
        | '(' Exp ',' Exp ')'          { LPair $2 $4 }

Type    :: { Type }
        : TYPEE                        { EmptyT }
        | TYPEU                        { UnitT }
        | Type '->' Type               { FunT $1 $3 }
        | '(' Type ')'                 { $2 }
        | '(' Type ',' Type ')'        { PairT $2 $4 }
        | TYPEN                        { NatT }

Defs    : Defexp Defs                  { $1 : $2 }
        |                              { [] }

{

data ParseResult a = Ok a | Failed String
                     deriving Show
type LineNumber = Int
type P a = String -> LineNumber -> ParseResult a

getLineNo :: P LineNumber
getLineNo = \s l -> Ok l

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l-> case m s l of
                         Ok a     -> k a s l
                         Failed e -> Failed e

returnP :: a -> P a
returnP a = \s l-> Ok a

failP :: String -> P a
failP err = \s l -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l -> case m s l of
                        Ok a     -> Ok a
                        Failed e -> k e s l

happyError :: P a
happyError = \ s i -> Failed $ "Línea "++(show (i::LineNumber))++": Error de parseo\n"++(s)

data Token = TVar String
               | TTypeE
               | TTypeU
               | TTypeN
               | TDef
               | TAbs
               | TDot
               | TOpen
               | TClose
               | TColon
               | TArrow
               | TEquals
               | TEOF
               | TLet
               | TIn
               | TAs
               | TUnit
               | TFst
               | TSnd
               | TComma
               | TZero
               | TSuc
               | TRec
               deriving Show

----------------------------------
lexer cont s = case s of
                    [] -> cont TEOF []
                    ('\n':s)  ->  \line -> lexer cont s (line + 1)
                    (c:cs)
                          | isSpace c -> lexer cont cs
                          | isAlpha c -> lexVar (c:cs)
                    ('-':('-':cs)) -> lexer cont $ dropWhile ((/=) '\n') cs
                    ('{':('-':cs)) -> consumirBK 0 0 cont cs
                    ('-':('}':cs)) -> \ line -> Failed $ "Línea "++(show line)++": Comentario no abierto"
                    ('-':('>':cs)) -> cont TArrow cs
                    ('\\':cs)-> cont TAbs cs
                    ('.':cs) -> cont TDot cs
                    ('(':cs) -> cont TOpen cs
                    ('-':('>':cs)) -> cont TArrow cs
                    (')':cs) -> cont TClose cs
                    (':':cs) -> cont TColon cs
                    ('=':cs) -> cont TEquals cs
                    (',':cs) -> cont TComma cs
                    ('0':cs) -> cont TZero cs
                    unknown -> \line -> Failed $
                     "Línea "++(show line)++": No se puede reconocer "++(show $ take 10 unknown)++ "..."
                    where lexVar cs = case span isAlpha cs of
                              ("E",rest)    -> cont TTypeE rest
                              ("Unit",rest) -> cont TTypeU rest
                              ("Nat",rest)  -> cont TTypeN rest
                              ("def",rest)  -> cont TDef rest
                              ("let",rest)  -> cont TLet rest
                              ("in", rest)  -> cont TIn rest
                              ("as", rest)  -> cont TAs rest
                              ("unit",rest) -> cont TUnit rest
                              ("fst", rest) -> cont TFst rest
                              ("snd", rest) -> cont TSnd rest
                              ("suc", rest) -> cont TSuc rest
                              ("R", rest)   -> cont TRec rest
                              (var,rest)    -> cont (TVar var) rest
                          consumirBK anidado cl cont s = case s of
                              ('-':('-':cs)) -> consumirBK anidado cl cont $ dropWhile ((/=) '\n') cs
                              ('{':('-':cs)) -> consumirBK (anidado+1) cl cont cs
                              ('-':('}':cs)) -> case anidado of
                                                  0 -> \line -> lexer cont cs (line+cl)
                                                  _ -> consumirBK (anidado-1) cl cont cs
                              ('\n':cs) -> consumirBK anidado (cl+1) cont cs
                              (_:cs) -> consumirBK anidado cl cont cs

stmts_parse s = parseStmts s 1
stmt_parse s = parseStmt s 1
term_parse s = term s 1
}
