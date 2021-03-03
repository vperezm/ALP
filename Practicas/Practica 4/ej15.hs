{-# LANGUAGE RankNTypes #-}

double :: forall x. (x -> x) -> x -> x
double = \f x -> f (f x)

id :: forall x. x -> x
id = \x -> x

{----------------------------------------------------------------------------}

{-BOOLEANOS
Representación de Church:
true = \t f -> t
false = \t f -> f
-}

type MBool = forall x. x -> x -> x

mtrue :: MBool
mtrue = \t f -> t

mfalse :: MBool
mfalse = \t f -> f

mnot :: MBool -> MBool
mnot = \b -> \t f -> b f t

{-NATURALES
Numerales de Church:
c0 = \s z -> z
c1 = \s z -> s z
c2 = \s z -> s (s z)
-}

type MNat = forall x. (x -> x) -> x -> x

c0 :: MNat
c0 = \s z -> z

c1 :: MNat
c1 = \s z -> s z

c2 :: MNat
c2 = \s z -> s (s z)

csucc :: MNat -> MNat
csucc = \n -> \s z -> s (n s z)

{-LISTAS
-}
type List x = forall r. (x -> r -> r) -> r -> r

lnil :: forall x. List x
lnil = \c n -> n

lcons :: forall x. x -> List x -> List x
lcons = \hd tl -> \c n -> c hd (tl c n)

lnull :: forall x. List x -> MBool
lnull = \xs -> xs (\hd tl -> mfalse) mtrue

{----------------------------------------------------------------------------}
-- 10 b

quadruple :: forall x. (x -> x) -> x -> x
quadruple = \f x -> double (double f) x

{----------------------------------------------------------------------------}
-- 12

type PairNat = forall x. (MNat -> MNat -> x) -> x

-- a)
pairNat :: MNat -> MNat -> PairNat
pairNat = \x y -> \f -> f x y

-- b)
fstNat :: PairNat -> MNat
fstNat = \p -> p (\x y -> x)

-- c)
sndNat :: PairNat -> MNat
sndNat = \p -> p (\x y -> y)

{----------------------------------------------------------------------------}
-- 13
{-
cpred :: MNat -> MNat
cpred = \n -> fstNat (n (\p -> pairNat (sndNat p) (csucc (sndNat p))) (pairNat c0 c0))
-}
{----------------------------------------------------------------------------}
-- 14
{-
-- a)
lmap :: forall x z. List x -> (x -> z) -> List z
lmap = \xs f -> xs (\hd tl -> lcons (f hd) tl) lnil
-}
-- b)
{-
insert :: forall x. (x -> x -> MBool) -> List x -> x -> List x
insert = \f xs x -> xs (\hd tl -> (f x hd) (lcons x tl) tl) (lcons x lnil)
-}
