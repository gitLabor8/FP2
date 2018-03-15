-------------------------------------------------------------------------------
-- Exercise 5.2
-- Dion Scheper      -- s4437578
-- Max van Laarhoven -- s4547136
-- Frank Gerlings    -- s4384873
-------------------------------------------------------------------------------

> {-# LANGUAGE UnicodeSyntax #-}
>
> module RandomAccessList12
> where
> -- import Unicode

> -- Bin12 is a modification of Bin, which uses the {1,2}-system
> data Bin12  =  E | I Bin12 | II Bin12
>   deriving(Show)

> incr :: Bin12 -> Bin12
> incr E        = I E
> incr (I bin)  = II bin
> incr (II bin) = I (incr bin)

> type Pair elem = (elem, elem)

> data Sequ elem
>   = ENil
>   | ICons  elem      (Sequ (Pair elem))
>   | IICons elem elem (Sequ (Pair elem))
>   deriving(Show)
> -- Testing this with (!) is satisfying, in some kind of weird way.
> testSequ = ICons 0 (ICons (1,2) (ICons ((3,4),(5,6)) ENil))

Interfaces

> nil   ∷ Sequ elem
> nil = ENil

> cons  ∷ elem → Sequ elem → Sequ elem
> cons elt ENil                  = ICons elt ENil
> cons elt (ICons  elt1      as) = IICons elt elt1 as
> cons elt (IICons elt1 elt2 as) = ICons elt (cons (elt1, elt2) as)

> head  ∷ Sequ elem → elem
> head ENil                  = error "Can't take head of empty sequence"
> head (ICons  elt1      as) = elt1
> head (IICons elt1 elt2 as) = elt1

> tail  ∷ Sequ elem → Sequ elem
> tail ENil                  = error "Can't take tail of empty sequence"
> tail (ICons  elt1      as) = flatten as
> tail (IICons elt1 elt2 as) = flatten as

> flatten :: Sequ (elem, elem) -> Sequ elem
> flatten ENil                                  = ENil
> flatten (ICons  (elt1, elt2)              as) = IICons elt1 elt2 (flatten as)
> flatten (IICons (elt1, elt2) (elt3, elt4) as) = IICons elt1 elt2 (cons (elt3, elt4) (flatten as))

> -- This function seems like it is programmable in a shorter/more elegant fashion. Not sure though...
> (!)   ∷ Sequ elem → Integer → elem
> (!) ENil                  _ = error "Can't take nth element of empty sequence"
> (!) (ICons  elt1      as) 0 = elt1
> (!) (IICons elt1 elt2 as) 0 = elt1
> (!) (ICons  elt1      as) n = (!) (flatten as) (n-1)
> (!) (IICons elt1 elt2 as) 1 = elt2
> (!) (IICons elt1 elt2 as) n = (!) (flatten as) (n-2)

A n-digit expression in {1,2}-binary number system in general needs less digits to represent the same numbers as in a {0,1}-binary number system.
We can see this is the modified sequ: to store 4 elements, we only need two nested instances of Sequ, rather than 3.
Note: Due to the high amount of `flatten` calls, this datastructure could still be very inefficient.
