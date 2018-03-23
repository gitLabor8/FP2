-------------------------------------------------------------------------------
-- Exercise 6.1
-- Dion Scheper      -- s4437578
-- Max van Laarhoven -- s4547136
-- Frank Gerlings    -- s4384873
-------------------------------------------------------------------------------

> {-# LANGUAGE UnicodeSyntax #-}
>
> module Foldr
> where
> -- import Unicode

> data List elem list  =  Nil | Cons elem list

> fold ∷ (List elem ans → ans) → ([elem] → ans)
> fold alg  =  consume
>   where  consume []        =  alg Nil
>          consume (x : xs)  =  alg (Cons x (consume xs))

1. Define foldr in terms of fold.
We use "foldr'" instead of "foldr" to ~introduce~ avoid naming conflicts

> foldr' :: (elem -> ans -> ans) -> ans -> ([elem] -> ans)
> foldr' (^$^) e = fold (\x -> case x of
>                               Nil    -> e
>                               Cons a -> a ^$^ s)

2. Conversely, define fold in terms of foldr
We use "fold'" instead of "fold" to ~introduce~ avoid naming conflicts

< fold' :: (elem -> ans -> ans) -> ([elem] -> ans)
< fold' alg = foldr (\ele an -> alg (Cons ele an)) (alg Nil)

(alg Nil) needs one more argument, but I don't seem to have any :(
Time to do another approach, that has the same problem:

> fold'' ∷ (List elem ans → ans) → ([elem] → ans)
> fold'' alg  =  foldr (\ele an -> alg (ele ) (alg Nil)


Werkcollege notes:
It carries the idea presented in the hoorcollege of
X^(A+B) =~= X^A * X^B
Being equivalent/isomorphic to:
Either a b =~= ((->) a, (->) b)

Oh and it is not asked by the excercise

< to :: (Either a b -> x) -> (a -> x, b -> x)
< to f = (\a -> f (Left a), \b -> f (Right b))

< from :: (a -> x, b -> x) ->  (Either a b -> x)
< from (fa, fb) = \ab -> case ab of
<                           Left  a -> fa a
<                           Right b -> fb b
