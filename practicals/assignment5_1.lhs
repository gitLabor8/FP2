-------------------------------------------------------------------------------
-- Exercise 5.1
-- Dion Scheper      -- s4437578
-- Max van Laarhoven -- s4547136
-- Frank Gerlings    -- s4384873
-------------------------------------------------------------------------------

> {-# LANGUAGE UnicodeSyntax #-}
>
> module RandomAccessList
> where
> -- import Unicode

> data Nat  =  Z | S Nat
>   deriving(Show)
> threeNat = S(S(S(Z)))

> data List elem  =  Zero | Succ elem (List elem)
>   deriving(Show)

> data Bin  =  N | O Bin | I Bin
>   deriving(Show)
> threeBin = I(I(O(N)))
> fourBin = O(O(I(N)))
> sixBin = O(I(I(N)))

> type Pair elem = (elem, elem)

> data Sequ elem
>   =  Nil
>   |  OCons       (Sequ (Pair elem))
>   |  ICons elem  (Sequ (Pair elem))
>   deriving(Show)

Exercise 1: The dank trashcan (get it?)

> -- From trashcan to dankness
> unary   ∷ Bin  → Nat
> unary N         = Z
> unary (O bin)   = twice (unary bin)
> unary (I bin)   = S (twice (unary bin))

> twice :: Nat -> Nat
> twice Z         = Z
> twice (S nat)   = S(S(twice nat))

> -- Puts dankness in the trashcan
> binary  ∷ Nat  → Bin
> binary Z       = N
> binary (S nat) = incr (binary nat)

> incr :: Bin -> Bin
> incr N     = I N
> incr (O bin) = I bin
> incr (I bin) = O (incr bin)

Exercise 2

< data Nat        =  Z    | S         Nat
< data List elem  =  Zero | Succ elem (List elem)

< data Bin
<   =  N
<   |  O           Bin
<   |  I           Bin
< data Sequ elem
<   =  Nil
<   |  OCons       (Sequ (Pair elem))
<   |  ICons elem  (Sequ (Pair elem))


> toList    ∷  Sequ elem  → List elem
> toList Nil           = Zero
> toList (OCons sequ)   = twice (toList sequ)
> toList (ICons extraArg sequ)   =  (twice (toList sequ))

> twice :: List elem -> List elem
> twice Zero         = Zero
> twice (S nat)   = S(S(twice nat))




< fromList  ∷  List elem  → Sequ elem
