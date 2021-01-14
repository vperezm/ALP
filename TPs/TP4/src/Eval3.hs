module Eval3
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

-- Ejercicio 3.a: Proponer una nueva mónada que lleve el costo de las
-- operaciones efectuadas en la computación, además de manejar errores y
-- estado, y de su instancia de mónada. Llámela StateErrorCost
newtype StateErrorCost a =
  StateErrorCost { runStateErrorCost :: Env -> Cost -> Either Error (Pair a (Env, Cost)) }

-- Para calmar al GHC
instance Functor StateErrorCost where
  fmap = liftM

instance Applicative StateErrorCost where
  pure  = return
  (<*>) = ap

instance Monad StateErrorCost where
  return x = StateErrorCost (\s -> (\c -> Right (x :!: (s, c))))
  m >>= f = StateErrorCost (\s -> (\c -> case runStateErrorCost m s c of
                                           Left r                 -> Left r
                                           Right (v :!: (s', c')) -> runStateErrorCost (f v) s' c'))

-- Ejercicio 3.c: Dar una instancia de MonadCost para StateErrorCost
instance MonadCost StateErrorCost where
  addWork n = StateErrorCost (\s -> (\c -> Right (() :!: (s, c + n))))

-- Ejercicio 3.d: Dar una instancia de MonadError para StateErrorCost
instance MonadError StateErrorCost where
  throw DivByZero = StateErrorCost (\_ -> (\_ -> Left DivByZero))
  throw UndefVar  = StateErrorCost (\_ -> (\_ -> Left UndefVar))

-- Ejercicio 3.e: Dar una instancia de MonadState para StateErrorCost
instance MonadState StateErrorCost where
  lookfor v = StateErrorCost (\s -> (\c -> case M.lookup v s of
                                             Nothing -> Left UndefVar
                                             Just n  -> Right (n :!: (s, c))))
  update v i = StateErrorCost (\s -> (\c -> Right ((() :!: (update' v i s, c)))))
                 where update' = M.insert

-- Ejercicio 3.f: Implementar el evaluador utilizando la mónada StateErrorCost

-- Evalúa un programa en el estado nulo
eval :: Comm -> Either Error (Env, Cost)
eval p = case runStateErrorCost (stepCommStar p) initEnv 0 of
           Left r               -> Left r
           Right (n :!: (s, c)) -> Right (s, c)

-- Evalúa múltiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m, MonadCost m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalúa un paso de un comando
stepComm :: (MonadState m, MonadError m, MonadCost m) => Comm -> m Comm
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
unOp :: (MonadState m, MonadError m, MonadCost m) => (a -> a) -> Exp a -> m a
unOp f e = do n <- evalExp e
              addWork 1
              return (f n)
-- Operadores binarios (con el trabajo a sumar como parámetro)
binOp :: (MonadState m, MonadError m, MonadCost m) => (a -> a -> b) -> Exp a -> Exp a -> Cost -> m b
binOp f e0 e1 n = do n0 <- evalExp e0
                     n1 <- evalExp e1
                     addWork n
                     return (f n0 n1)
-- Operador para división
divv :: (MonadState m, MonadError m, MonadCost m) => Exp Int -> Exp Int -> m Int
divv e0 e1 = do n0 <- evalExp e0
                n1 <- evalExp e1
                if n1 == 0 then throw DivByZero
                           else do addWork 2
                                   return (div n0 n1)
-- Operador para secuencia de expresiones enteras
seqq :: Int -> Int -> Int
seqq n0 n1 = n1

evalExp :: (MonadState m, MonadError m, MonadCost m) => Exp a -> m a
-- Expresiones enteras
evalExp (Const n)     = return n
evalExp (Var v)       = lookfor v
evalExp (UMinus e)    = unOp negate e
evalExp (Plus e0 e1)  = binOp (+) e0 e1 1
evalExp (Minus e0 e1) = binOp (-) e0 e1 1
evalExp (Times e0 e1) = binOp (*) e0 e1 2
evalExp (Div e0 e1)   = divv      e0 e1
evalExp (EAssgn v e)  = do n <- evalExp e
                           update v n
                           return n
evalExp (ESeq e0 e1)  = binOp seqq e0 e1 0
-- Expresiones booleanas
evalExp BTrue         = return True
evalExp BFalse        = return False
evalExp (Lt e0 e1)    = binOp (<)  e0 e1 1
evalExp (Gt e0 e1)    = binOp (>)  e0 e1 1
evalExp (And p0 p1)   = binOp (&&) p0 p1 1
evalExp (Or p0 p1)    = binOp (||) p0 p1 1
evalExp (Not p)       = unOp  not    p
evalExp (Eq e0 e1)    = binOp (==) e0 e1 1
evalExp (NEq e0 e1)   = binOp (/=) e0 e1 1
