> {-# LANGUAGE UnicodeSyntax #-}
>
> module Generate
> where
> -- import Unicode

> bools  ∷  [Bool]
> bools  =  pure False ++ pure True
>
> maybes  ∷  [elem] → [Maybe elem]
> maybes elems  =  pure Nothing ++ (pure Just <*> elems)

> data Suit  =  Spades | Hearts | Diamonds | Clubs
> data Rank  =  Faceless Integer | Jack | Queen | King
> data Card  =  Card Rank Suit | Joker

> data Tree elem = Empty | Node (Tree elem) elem (Tree elem)

lists bools 1
lists bools 2
trees (lists bools 2) 1
trees (lists bools 2) 2
