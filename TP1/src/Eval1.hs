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
stepCommStar c    s = T.uncurry stepCommStar $ stepComm c s

-- Evalúa un paso de un comando en un estado dado
stepComm :: Comm -> State -> Pair Comm State
stepComm Skip                 s = (Skip :!: s)
stepComm (Let v e)            s = let (n :!: s') = evalExp e s
                                  in (Skip :!: update v n s')
stepComm (Seq Skip c1)        s = (c1 :!: s)
stepComm (Seq c0 c1)          s = let (c0' :!: s') = stepComm c0 s
                                  in (Seq c0' c1 :!: s')
stepComm (IfThenElse b c0 c1) s = let (b' :!: s') = evalExp b s
                                  in if b' then (c0 :!: s') else (c1 :!: s')
stepComm (While b c)          s = let (b' :!: s') = evalExp b s
                                  in if b' then (Seq c (While b c) :!: s') else (Skip :!: s')

-- Evalúa una expresión
evalExp :: Exp a -> State -> Pair a State
-- Expresiones enteras
evalExp (Const n)     s = (n :!: s)
evalExp (Var v)       s = ((lookfor v s) :!: s)
evalExp (UMinus e)    s = let (n :!: s') = evalExp e s
                          in (-n :!: s')
evalExp (Plus e0 e1)  s = let (n0 :!: s')  = evalExp e0 s
                              (n1 :!: s'') = evalExp e1 s'
                          in (n0 + n1 :!: s'')
evalExp (Minus e0 e1) s = let (n0 :!: s')  = evalExp e0 s
                              (n1 :!: s'') = evalExp e1 s'
                          in (n0 - n1 :!: s'')
evalExp (Times e0 e1) s = let (n0 :!: s')  = evalExp e0 s
                              (n1 :!: s'') = evalExp e1 s'
                          in (n0 * n1 :!: s'')
evalExp (Div e0 e1)   s = let (n0 :!: s')  = evalExp e0 s
                              (n1 :!: s'') = evalExp e1 s'
                          in (n0 `div` n1 :!: s'')
evalExp (EAssgn v e)  s = let (n :!: s') = evalExp e s
                          in (n :!: update v n s')
evalExp (ESeq e0 e1)  s = let (n0 :!: s')  = evalExp e0 s
                              (n1 :!: s'') = evalExp e1 s'
                          in (n1 :!: s'')
-- Expresiones booleanas
evalExp BTrue         s = (True :!: s)
evalExp BFalse        s = (False :!: s)
evalExp (Lt e0 e1)    s = let (n0 :!: s')  = evalExp e0 s
                              (n1 :!: s'') = evalExp e1 s'
                          in (n0 < n1 :!: s'')
evalExp (Gt e0 e1)    s = let (n0 :!: s')  = evalExp e0 s
                              (n1 :!: s'') = evalExp e1 s'
                          in (n0 > n1 :!: s'')
evalExp (And p0 p1)   s = let (b0 :!: s')  = evalExp p0 s
                              (b1 :!: s'') = evalExp p1 s'
                          in (b0 && b1 :!: s'')
evalExp (Or p0 p1)    s = let (b0 :!: s')  = evalExp p0 s
                              (b1 :!: s'') = evalExp p1 s'
                          in ((b0 || b1) :!: s'')
evalExp (Not p)       s = let (b :!: s') = evalExp p s
                          in (not b :!: s')
evalExp (Eq e0 e1)    s = let (n0 :!: s')  = evalExp e0 s
                              (n1 :!: s'') = evalExp e1 s'
                          in (n0 == n1 :!: s'')
evalExp (NEq e0 e1)   s = let (n0 :!: s')  = evalExp e0 s
                              (n1 :!: s'') = evalExp e1 s'
                          in (not (n0 == n1) :!: s'')
