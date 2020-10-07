module Eval1
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados
type State = M.Map Variable Int

-- Estado nulo
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Int
lookfor v s = case M.lookup v s of
                Just n -> n

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalúa un programa en el estado nulo
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comando en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalúa un paso de un comando en un estado dado
stepComm :: Comm -> State -> Pair Comm State
stepComm Skip                 s = (Skip :!: s)
stepComm (Let v e)            s = let (n :!: s')   = evalExp e s
                                  in (Skip :!: update v n s')
stepComm (Seq Skip c1)        s = (c1 :!: s)
stepComm (Seq c0 c1)          s = let (c0' :!: s') = stepComm c0 s
                                  in (Seq c0' c1 :!: s')
stepComm (IfThenElse b c0 c1) s = let (b' :!: s')  = evalExp b s
                                  in if b'
                                     then (c0 :!: s')
                                     else (c1 :!: s')
stepComm (While b c)          s = let (b' :!: s')  = evalExp b s
                                  in if b'
                                     then (Seq c (While b c) :!: s')
                                     else (Skip :!: s')

-- Evalúa una expresión

-- Funciones auxiliares:
-- Operadores unarios
unOp :: (a -> a) -> Exp a -> State -> Pair a State
unOp f e s = let (n :!: s') = evalExp e s
             in (f n :!: s')
-- Operadores binarios
binOp :: (a -> a -> b) -> Exp a -> Exp a -> State -> Pair b State
binOp f e0 e1 s = let (n0 :!: s')  = evalExp e0 s
                      (n1 :!: s'') = evalExp e1 s'
                  in (f n0 n1 :!: s'')
-- Operador para secuencia de enteros
seqq :: Int -> Int -> Int
seqq n0 n1 = n1

evalExp :: Exp a -> State -> Pair a State
-- Expresiones enteras
evalExp (Const n)     s = (n :!: s)
evalExp (Var v)       s = ((lookfor v s) :!: s)
evalExp (UMinus e)    s = unOp negate  e   s
evalExp (Plus e0 e1)  s = binOp (+)  e0 e1 s
evalExp (Minus e0 e1) s = binOp (-)  e0 e1 s
evalExp (Times e0 e1) s = binOp (*)  e0 e1 s
evalExp (Div e0 e1)   s = binOp div  e0 e1 s
evalExp (EAssgn v e)  s = let (n :!: s') = evalExp e s
                          in (n :!: update v n s')
evalExp (ESeq e0 e1)  s = binOp seqq e0 e1 s
-- Expresiones booleanas
evalExp BTrue         s = (True :!: s)
evalExp BFalse        s = (False :!: s)
evalExp (Lt e0 e1)    s = binOp (<)  e0 e1 s
evalExp (Gt e0 e1)    s = binOp (>)  e0 e1 s
evalExp (And p0 p1)   s = binOp (&&) p0 p1 s
evalExp (Or p0 p1)    s = binOp (||) p0 p1 s
evalExp (Not p)       s = unOp  not    p   s
evalExp (Eq e0 e1)    s = binOp (==) e0 e1 s
evalExp (NEq e0 e1)   s = binOp (/=) e0 e1 s
