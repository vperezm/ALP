module Eval1
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple             as T

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
stepComm (Let v e)            s = (Skip :!: update v (T.fst (evalExp e s)) (T.snd (evalExp e s)))
stepComm (Seq Skip c1)        s = (c1 :!: s)
stepComm (Seq c0 c1)          s = (Seq (T.fst (stepComm c0 s)) c1 :!: (T.snd (stepComm c0 s)))
stepComm (IfThenElse b c0 c1) s = if (T.fst (evalExp b s)) then (c0 :!: (T.snd (evalExp b s))) else (c1 :!: (T.snd (evalExp b s)))
stepComm (While b c)          s = if (T.fst (evalExp b s)) then ((Seq c (While b c)) :!: (T.snd (evalExp b s))) else (Skip :!: (T.snd (evalExp b s)))

-- Evalúa una expresión
evalExp :: Exp a -> State -> Pair a State
evalExp (Const n)     s = (n :!: s)
evalExp (Var v)       s = ((lookfor v s) :!: s)
evalExp (UMinus e)    s = (-(T.fst (evalExp e s)) :!: T.snd (evalExp e s))
evalExp (Plus e0 e1)  s = ((T.fst (evalExp e0 s)) + (T.fst (evalExp e1 (T.snd (evalExp e0 s)))) :!: T.snd (evalExp e1 (T.snd (evalExp e0 s))))
evalExp (Minus e0 e1) s = ((T.fst (evalExp e0 s)) - (T.fst (evalExp e1 (T.snd (evalExp e0 s)))) :!: T.snd (evalExp e1 (T.snd (evalExp e0 s))))
evalExp (Times e0 e1) s = ((T.fst (evalExp e0 s)) * (T.fst (evalExp e1 (T.snd (evalExp e0 s)))) :!: T.snd (evalExp e1 (T.snd (evalExp e0 s))))
evalExp (Div e0 e1)   s = ((T.fst (evalExp e0 s)) `div` (T.fst (evalExp e1 (T.snd (evalExp e0 s)))) :!: T.snd (evalExp e1 (T.snd (evalExp e0 s))))
evalExp (EAssgn v e)  s = (T.fst (evalExp e s) :!: update v (T.fst (evalExp e s)) (T.snd (evalExp e s)))
evalExp (ESeq e0 e1)  s = evalExp e1 (T.snd (evalExp e0 s))
evalExp BTrue         s = (True :!: s)
evalExp BFalse        s = (False :!: s)
evalExp (Lt e0 e1)    s = if (T.fst (evalExp e0 s)) < (T.fst (evalExp e1 (T.snd (evalExp e0 s)))) then (True :!: T.snd (evalExp e1 (T.snd (evalExp e0 s)))) else (False :!: T.snd (evalExp e1 (T.snd (evalExp e0 s))))
evalExp (Gt e0 e1)    s = if (T.fst (evalExp e0 s)) > (T.fst (evalExp e1 (T.snd (evalExp e0 s)))) then (True :!: T.snd (evalExp e1 (T.snd (evalExp e0 s)))) else (False :!: T.snd (evalExp e1 (T.snd (evalExp e0 s))))
evalExp (And p0 p1)   s = ((T.fst (evalExp p0 s)) && (T.fst (evalExp p1 (T.snd (evalExp p0 s)))) :!: T.snd (evalExp p1 (T.snd (evalExp p0 s))))
evalExp (Or p0 p1)    s = (((T.fst (evalExp p0 s)) || (T.fst (evalExp p1 (T.snd (evalExp p0 s))))) :!: T.snd (evalExp p1 (T.snd (evalExp p0 s))))
evalExp (Not p)       s = (not (T.fst (evalExp p s)) :!: T.snd (evalExp p s))
evalExp (Eq e0 e1)    s = if (T.fst (evalExp e0 s)) == (T.fst (evalExp e1 (T.snd (evalExp e0 s)))) then (True :!: T.snd (evalExp e1 (T.snd (evalExp e0 s)))) else (False :!: T.snd (evalExp e1 (T.snd (evalExp e0 s))))
evalExp (NEq e0 e1)   s = if (T.fst (evalExp e0 s)) /= (T.fst (evalExp e1 (T.snd (evalExp e0 s)))) then (True :!: T.snd (evalExp e1 (T.snd (evalExp e0 s)))) else (False :!: T.snd (evalExp e1 (T.snd (evalExp e0 s))))
