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
-- COMPLETAR

-- Recuerde agregar las siguientes instancias para calmar al GHC:
-- instance Functor StateErrorCost where
--   fmap = liftM

-- instance Applicative StateErrorCost where
--   pure  = return
--   (<*>) = ap

-- Ejercicio 3.c: Dar una instancia de MonadCost para StateErrorCost
-- COMPLETAR

-- Ejercicio 3.d: Dar una instancia de MonadError para StateErrorCost
-- COMPLETAR

-- Ejercicio 3.e: Dar una instancia de MonadState para StateErrorCost
-- COMPLETAR

-- Ejercicio 3.f: Implementar el evaluador utilizando la mónada StateErrorCost

-- Evalúa un programa en el estado nulo
eval :: Comm -> Either Error (Env, Cost)
eval = undefined

-- Evalúa múltiples pasos de un comando, hasta alcanzar un Skip
-- stepCommStar :: [dar el tipo según corresponda]
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalúa un paso de un comando
-- stepComm :: [dar el tipo según corresponda]
stepComm = undefined

-- Evalúa una expresión
-- evalIntExp :: [dar el tipo según corresponda]
evalExp = undefined
