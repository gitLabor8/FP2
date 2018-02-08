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
