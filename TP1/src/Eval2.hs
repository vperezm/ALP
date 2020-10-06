module Eval2
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
lookfor :: Variable -> State -> Either Error Int
lookfor v s = case M.lookup v s of
                Just n  -> Right n
                Nothing -> error ("Variable \"" ++ v ++ "\" no definida")

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalúa un programa en el estado nulo
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comando en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'

-- Evalúa un paso de un comando en un estado dado
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm Skip                 s = Right (Skip :!: s)
stepComm (Let v e)            s = case evalExp e s of
                                    Left r           -> Left r
                                    Right (n :!: s') -> Right (Skip :!: update v n s')
stepComm (Seq Skip c1)        s = Right (c1 :!: s)
stepComm (Seq c0 c1)          s = case stepComm c0 s of
                                    Left r             -> Left r
                                    Right (c0' :!: s') -> Right (Seq c0' c1 :!: s')
stepComm (IfThenElse b c0 c1) s = case evalExp b s of
                                    Left r               -> Left r
                                    Right (True  :!: s') -> Right (c0 :!: s')
                                    Right (False :!: s') -> Right (c1 :!: s')
stepComm (While b c)          s = case evalExp b s of
                                    Left r               -> Left r
                                    Right (True  :!: s') -> Right (Seq c (While b c) :!: s')
                                    Right (False :!: s') -> Right (Skip :!: s')

-- Evalúa una expresión

-- Funcioes auxiliares:
-- Operadores binarios
binOp :: (a -> a -> b) -> Exp a -> Exp a -> State -> Either Error (Pair b State)
binOp f e0 e1 s = case evalExp e0 s of
                    Left r            -> Left r
                    Right (n0 :!: s') -> case evalExp e1 s' of
                                           Left r             -> Left r
                                           Right (n1 :!: s'') -> Right (f n0 n1 :!: s'')
-- División
divv :: Int -> Int -> Int
divv n0 n1 = case n1 of
             0 -> error "División por 0"
             _ -> n0 `div` n1
-- Secuencia de expresiones enteras
seqq :: Int -> Int -> Int
seqq n0 n1 = n1

evalExp :: Exp a -> State -> Either Error (Pair a State)
-- Expresiones enteras
evalExp (Const n)     s = Right (n :!: s)
evalExp (Var v)       s = case lookfor v s of
                            Left _  -> Left UndefVar
                            Right n -> Right (n :!: s)
evalExp (UMinus e)    s = case evalExp e s of
                            Left r           -> Left r
                            Right (n :!: s') -> Right (-n :!: s')
evalExp (Plus e0 e1)  s = binOp (+)  e0 e1 s
evalExp (Minus e0 e1) s = binOp (-)  e0 e1 s
evalExp (Times e0 e1) s = binOp (*)  e0 e1 s
evalExp (Div e0 e1)   s = binOp divv e0 e1 s
evalExp (EAssgn v e)  s = case evalExp e s of
                            Left r           -> Left r
                            Right (n :!: s') -> Right (n :!: update v n s')
evalExp (ESeq e0 e1)  s = binOp seqq e0 e1 s
-- Expresiones booleanas
evalExp BTrue         s = Right (True :!: s)
evalExp BFalse        s = Right (False :!: s)
evalExp (Lt e0 e1)    s = binOp (<)  e0 e1 s
evalExp (Gt e0 e1)    s = binOp (>)  e0 e1 s
evalExp (And p0 p1)   s = binOp (&&) p0 p1 s
evalExp (Or p0 p1)    s = binOp (||) p0 p1 s
evalExp (Not p)       s = case evalExp p s of
                            Left r           -> Left r
                            Right (b :!: s') -> Right (not b :!: s')
evalExp (Eq e0 e1)    s = binOp (==) e0 e1 s
evalExp (NEq e0 e1)   s = binOp (/=) e0 e1 s
