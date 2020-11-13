module Simplytyped
  ( conversion
  ,    -- conversión a términos localmente sin nombres
    eval
  ,          -- evaluador
    infer
  ,         -- inferidor de tipos
    quote          -- valores -> términos
  )
where

import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( (>>=) )
import           Text.PrettyPrint.HughesPJ      ( render )
import           PrettyPrinter
import           Common

-- conversión a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion = conversion' []

conversion' :: [String] -> LamTerm -> Term
conversion' b (LVar n)     = maybe (Free (Global n)) Bound (n `elemIndex` b)
conversion' b (LApp t u)   = conversion' b t :@: conversion' b u
conversion' b (LAbs n t u) = Lam t (conversion' (n : b) u)
conversion' b (LLet n t u) = Let (conversion' (n : b) t) (conversion' (n : b) u)
conversion' b (LAs u t)    = As (conversion' b u) t
conversion' b LUnit        = Unit
conversion' b (LPair t u)  = Pair (conversion' b t) (conversion' b u)
conversion' b (LFst t)     = Fst (conversion' b t)
conversion' b (LSnd t)     = Snd (conversion' b t)

-----------------------
--- eval
-----------------------

-- sub i u1 u2 = u2[u1/i]
sub :: Int -> Term -> Term -> Term
sub i t (Bound j) | i == j    = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _ (Free n)              = Free n
sub i t (u :@: v)             = sub i t u :@: sub i t v
sub i t (Lam t' u)            = Lam t' (sub (i + 1) t u)
sub i t (Let t1 t2)           = Let (sub (i + 1) t t1) (sub (i + 1) t t2)
sub i t (As u t')             = As (sub i t u) t'
sub i t Unit                  = Unit
sub i t (Pair t1 t2)          = Pair (sub i t t1) (sub i t t2)
sub i t (Fst u)               = Fst (sub i t u)
sub i t (Snd u)               = Snd (sub i t u)

-- evaluador de términos
eval :: NameEnv Value Type -> Term -> Value
eval _ (Bound _)             = error "variable ligada inesperada en eval"
eval e (Free n)              = fst $ fromJust $ lookup n e
eval _ (Lam t u)             = VLam t u
eval e (Lam _ u :@: Lam s v) = eval e (sub 0 (Lam s v) u)
eval e (Lam t u1 :@: u2)     = let v2 = eval e u2 in eval e (sub 0 (quote v2) u1)
eval e (u :@: v)             = case eval e u of
  VLam t u' -> eval e (Lam t u' :@: v)
  _         -> error "Error de tipo en run-time, verificar type checker"
eval e (Let u1 u2)           = eval e (sub 0 u1 u2)
eval e (As u t)              = eval e u
eval e Unit                  = VUnit
eval e (Pair u1 u2)          = VPair (eval e u1) (eval e u2)
eval e (Fst u)               = eval e u
eval e (Snd u)               = eval e u

-----------------------
--- quoting
-----------------------

quote :: Value -> Term
quote (VLam t f)    = Lam t f
quote VUnit         = Unit
quote (VPair v1 v2) = Pair (quote v1) (quote v2)

-----------------------
--- type checker
-----------------------

-- type checker
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []

-- definiciones auxiliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=)
  :: Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v

-- fcs. de error
matchError :: Type -> Type -> Either String Type
matchError t1 t2 =
  err
    $  "se esperaba "
    ++ render (printType t1)
    ++ ", pero "
    ++ render (printType t2)
    ++ " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' c _ (Bound i)    = ret (c !! i)
infer' _ e (Free  n)    = case lookup n e of
  Nothing     -> notfoundError n
  Just (_, t) -> ret t
infer' c e (t :@: u)    = infer' c e t >>= \tt -> infer' c e u >>= \tu ->
  case tt of
    FunT t1 t2 -> if (tu == t1) then ret t2 else matchError t1 tu
    _          -> notfunError tt
infer' c e (Lam t u)    = infer' (t : c) e u >>= \tu -> ret $ FunT t tu
infer' c e (Let u1 u2)  = infer' c e u1 >>= \tu1 -> infer' (tu1:c) e u2 >>= \tu2 -> ret tu2
infer' c e (As u t)     = infer' c e u >>= \tu -> if tu == t then ret t else matchError t tu
infer' c e Unit         = ret UnitT
infer' c e (Pair u1 u2) = infer' c e u1 >>= \tu1 -> infer' c e u2 >>= \tu2 -> ret $ PairT tu1 tu2
infer' c e (Fst u)      = infer' c e u
infer' c e (Snd u)      = infer' c e u
----------------------------------
