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
                Nothing -> Left UndefVar

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
-- Completar la definición
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm = undefined

-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp (Const n)     s = Right (n :!: s)
evalExp (Var v)       s = case lookfor v s of
                            Left _  -> Left UndefVar
                            Right n -> Right (n :!: s)
evalExp (UMinus e)    s = case evalExp e s of
                            Left r  -> Left r
                            Right n -> Right (-(Data.Strict.Tuple.fst n) :!: Data.Strict.Tuple.snd n)
evalExp (Plus e0 e1)  s = case evalExp e0 s of
                            Left r   -> Left r
                            Right n0 -> case evalExp e1 (Data.Strict.Tuple.snd n0) of
                                          Left r   -> Left r
                                          Right n1 -> Right ((Data.Strict.Tuple.fst n0) + (Data.Strict.Tuple.fst n1) :!: Data.Strict.Tuple.snd n1)
evalExp (Minus e0 e1) s = case evalExp e0 s of
                            Left r   -> Left r
                            Right n0 -> case evalExp e1 (Data.Strict.Tuple.snd n0) of
                                          Left r   -> Left r
                                          Right n1 -> Right ((Data.Strict.Tuple.fst n0) - (Data.Strict.Tuple.fst n1) :!: Data.Strict.Tuple.snd n1)
evalExp (Times e0 e1) s = case evalExp e0 s of
                            Left r   -> Left r
                            Right n0 -> case evalExp e1 (Data.Strict.Tuple.snd n0) of
                                          Left r   -> Left r
                                          Right n1 -> Right ((Data.Strict.Tuple.fst n0) * (Data.Strict.Tuple.fst n1) :!: Data.Strict.Tuple.snd n1)
evalExp (Div e0 e1)   s = case evalExp e0 s of
                            Left r   -> Left r
                            Right n0 -> case evalExp e1 (Data.Strict.Tuple.snd n0) of
                                          Left r   -> Left r
                                          Right n1 -> case Data.Strict.Tuple.fst n1 of
                                                        0 -> Left DivByZero
                                                        _ -> Right ((Data.Strict.Tuple.fst n0) `div` (Data.Strict.Tuple.fst n1) :!: Data.Strict.Tuple.snd n1)
evalExp (EAssgn v e)  s = case evalExp e s of
                            Left r  -> Left r
                            Right n -> Right (Data.Strict.Tuple.fst n :!: update v (Data.Strict.Tuple.fst n) (Data.Strict.Tuple.snd n))
evalExp (ESeq e0 e1)  s = case evalExp e0 s of
                            Left r   -> Left r
                            Right n0 -> case evalExp e1 (Data.Strict.Tuple.snd n0) of
                                          Left r   -> Left r
                                          Right n1 -> Right (Data.Strict.Tuple.fst n1 :!: Data.Strict.Tuple.snd n1)
