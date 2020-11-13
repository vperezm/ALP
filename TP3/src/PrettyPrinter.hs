module PrettyPrinter
  ( printTerm
  ,     -- pretty printer para términos
    printType     -- pretty printer para tipos
  )
where

import           Common
import           Text.PrettyPrint.HughesPJ
import           Prelude                 hiding ( (<>) )
-- lista de posibles nombres para variables
vars :: [String]
vars =
  [ c : n
  | n <- "" : map show [(1 :: Integer) ..]
  , c <- ['x', 'y', 'z'] ++ ['a' .. 'w']
  ]

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- pretty printer de términos
pp :: Int -> [String] -> Term -> Doc
pp ii vs (Bound k)          = text (vs !! (ii - k - 1))
pp _  _  (Free  (Global s)) = text s
pp ii vs (i :@: c)          = sep
  [ parensIf (isLam i || isLet i) (pp ii vs i)
  , nest 1 (parensIf (isLam c || isLet c || isApp c) (pp ii vs c))
  ]
pp ii vs (Lam t c) =
  text "\\"
    <> text (vs !! ii)
    <> text ":"
    <> printType t
    <> text ". "
    <> pp (ii + 1) vs c
pp ii vs (Let u1 u2) =
  text "let "
    <> text(vs !! ii)
    <> text " = "
    <> pp (ii + 1) vs u1
    <> text " in "
    <> pp (ii + 1) vs u2
pp ii vs (As u t) =
  pp ii vs u
    <> text " as "
    <> printType t
pp ii vs Unit = text "unit"
pp ii vs (Pair u1 u2) =
  text "("
    <> pp ii vs u1
    <> text ","
    <> pp ii vs u2
    <> text ")"
pp ii vs (Fst u) =
  text "fst "
    <> pp ii vs u
pp ii vs (Snd u) =
  text "snd "
    <> pp ii vs u


isLam :: Term -> Bool
isLam (Lam _ _) = True
isLam _         = False

isApp :: Term -> Bool
isApp (_ :@: _) = True
isApp _         = False

isLet :: Term -> Bool
isLet (Let _ _) = True
isLet _         = False

-- pretty printer de tipos
printType :: Type -> Doc
printType EmptyT = text "E"
printType UnitT  = text "Unit"
printType (FunT t1 t2) =
  sep [parensIf (isFun t1) (printType t1), text "->", printType t2]
printType (PairT t1 t2) =
  sep [text "(", printType t1, text ",", printType t2, text ")"]


isFun :: Type -> Bool
isFun (FunT _ _) = True
isFun _          = False

fv :: Term -> [String]
fv (Bound _)         = []
fv (Free (Global n)) = [n]
fv (t :@: u)         = fv t ++ fv u
fv (Lam _ u)         = fv u
fv (Let u1 u2)       = fv u1 ++ fv u2
fv (As u t)          = fv u
fv Unit              = []
fv (Pair u1 u2)      = fv u1 ++ fv u2
fv (Fst u)           = fv u
fv (Snd u)           = fv u

---
printTerm :: Term -> Doc
printTerm t = pp 0 (filter (\v -> not $ elem v (fv t)) vars) t
