module Eval2
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado, con manejo de errores
newtype StateError a =
  StateError { runStateError :: Env -> Either Error (Pair a Env) }

-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

-- Ejercicio 2.a: Dar una instancia de Monad para StateError:
instance Monad StateError where
  return x = StateError (\s -> Right (x :!: s))
  m >>= f = StateError (\s -> case runStateError m s of
                                Left r           -> Left r
                                Right (v :!: s') -> runStateError (f v) s')

-- Ejercicio 2.b: Dar una instancia de MonadError para StateError:
instance MonadError StateError where
  throw DivByZero = StateError (\_ -> Left DivByZero)
  throw UndefVar  = StateError (\_ -> Left UndefVar)

-- Ejercicio 2.c: Dar una instancia de MonadState para StateError:
instance MonadState StateError where
  lookfor v = StateError (\s -> case M.lookup v s of
                                  Nothing -> Left UndefVar
                                  Just n  -> Right (n :!: s))
  update v i = StateError (\s -> Right ((() :!: update' v i s))) where update' = M.insert

-- Ejercicio 2.d: Implementar el evaluador utilizando la mónada StateError

-- Evalúa un programa en el estado nulo
eval :: Comm -> Either Error Env
eval p = case runStateError (stepCommStar p) initEnv of
           Left r          -> Left r
           Right (n :!: s) -> Right s

-- Evalúa múltiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalúa un paso de un comando
stepComm :: (MonadState m, MonadError m) => Comm -> m Comm
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
unOp :: (MonadState m, MonadError m) => (a -> a) -> Exp a -> m a
unOp f e = do n <- evalExp e
              return (f n)
-- Operadores binarios
binOp :: (MonadState m, MonadError m) => (a -> a -> b) -> Exp a -> Exp a -> m b
binOp f e0 e1 = do n0 <- evalExp e0
                   n1 <- evalExp e1
                   return (f n0 n1)
-- Operador para división
divv :: (MonadState m, MonadError m) => Exp Int -> Exp Int -> m Int
divv e0 e1 = do n0 <- evalExp e0
                n1 <- evalExp e1
                if n1 == 0 then throw DivByZero
                           else return (div n0 n1)
-- Operador para secuencia de expresiones enteras
seqq :: Int -> Int -> Int
seqq n0 n1 = n1

evalExp :: (MonadState m, MonadError m) => Exp a -> m a
-- Expresiones enteras
evalExp (Const n)     = return n
evalExp (Var v)       = lookfor v
evalExp (UMinus e)    = unOp negate e
evalExp (Plus e0 e1)  = binOp (+) e0 e1
evalExp (Minus e0 e1) = binOp (-) e0 e1
evalExp (Times e0 e1) = binOp (*) e0 e1
evalExp (Div e0 e1)   = divv      e0 e1
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
