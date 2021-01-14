module Eval1
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Prelude                 hiding ( fst
                                                , snd
                                                )
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado
newtype State a = State { runState :: Env -> Pair a Env }

instance Monad State where
  return x = State (\s -> (x :!: s))
  m >>= f = State (\s -> let (v :!: s') = runState m s in runState (f v) s')

-- Para calmar al GHC
instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap

instance MonadState State where
  lookfor v = State (\s -> (lookfor' v s :!: s))
    where lookfor' v s = fromJust $ M.lookup v s
  update v i = State (\s -> (() :!: update' v i s)) where update' = M.insert

-- Ejercicio 1.b: Implementar el evaluador utilizando la mónada State

-- Evalúa un programa en el estado nulo
eval :: Comm -> Env
eval p = snd (runState (stepCommStar p) initEnv)

-- Evalúa múltiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: MonadState m => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalúa un paso de un comando
stepComm :: MonadState m => Comm -> m Comm
stepComm Skip                 = return Skip
stepComm (Let v e)            = do n <- evalExp e
                                   update v n
                                   return Skip
stepComm (Seq Skip c1)        = return c1
stepComm (Seq c0 c1)          = do c0' <- stepComm c0
                                   return (Seq c0' c1)
stepComm (IfThenElse b c0 c1) = do b' <- evalExp b
                                   if b' then return c0
                                         else return c1
stepComm (While b c)          = do b' <- evalExp b
                                   if b' then return (Seq c (While b c))
                                         else return Skip

-- Evalúa una expresión

-- Funciones auxiliares:
-- Operadores unarios
unOp :: MonadState m => (a -> a) -> Exp a -> m a
unOp f e = do n <- evalExp e
              return (f n)
-- Operadores binarios
binOp :: MonadState m => (a -> a -> b) -> Exp a -> Exp a -> m b
binOp f e0 e1 = do n0 <- evalExp e0
                   n1 <- evalExp e1
                   return (f n0 n1)
-- Operador para secuencia de expresiones enteras
seqq :: Int -> Int -> Int
seqq n0 n1 = n1

evalExp :: MonadState m => Exp a -> m a
-- Expresiones enteras
evalExp (Const n)     = return n
evalExp (Var v)       = lookfor v
evalExp (UMinus e)    = unOp negate e
evalExp (Plus e0 e1)  = binOp (+) e0 e1
evalExp (Minus e0 e1) = binOp (-) e0 e1
evalExp (Times e0 e1) = binOp (*) e0 e1
evalExp (Div e0 e1)   = binOp div e0 e1
evalExp (EAssgn v e)  = do n <- evalExp e
                           update v n
                           return n
evalExp (ESeq e0 e1)  = binOp seqq e0 e1
-- Expresiones booleanas
evalExp BTrue         = return True
evalExp BFalse        = return False
evalExp (Lt e0 e1)    = binOp (<)  e0 e1
evalExp (Gt e0 e1)    = binOp (>)  e0 e1
evalExp (And p0 p1)   = binOp (&&) p0 p1
evalExp (Or p0 p1)    = binOp (||) p0 p1
evalExp (Not p)       = unOp  not    p
evalExp (Eq e0 e1)    = binOp (==) e0 e1
evalExp (NEq e0 e1)   = binOp (/=) e0 e1
