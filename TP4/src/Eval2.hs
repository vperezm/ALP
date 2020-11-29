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
  StateError { runStateError :: Env -> Either Error ( Pair a Env) }

-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

-- Ejercicio 2.a: Dar una instancia de Monad para StateError:
instance Monad StateError where
  return x = undefined
  m >>= f = undefined

-- Ejercicio 2.b: Dar una instancia de MonadError para StateError:
-- COMPLETAR

-- Ejercicio 2.c: Dar una instancia de MonadState para StateError:
-- COMPLETAR

-- Ejercicio 2.d: Implementar el evaluador utilizando la mónada StateError

-- Evalúa un programa en el estado nulo
eval :: Comm -> Either Error Env
eval = undefined

-- Evalúa múltiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalúa un paso de un comando
stepComm :: (MonadState m, MonadError m) => Comm -> m Comm
stepComm = undefined

-- Evalúa una expresión
evalExp :: (MonadState m, MonadError m) => Exp a -> m a
evalExp = undefined
