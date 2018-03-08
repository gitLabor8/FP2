-------------------------------------------------------------------------------
-- Exercise 4.3
-- Dion Scheper      -- s4437578
-- Max van Laarhoven -- s4547136
-- Frank Gerlings    -- s4384873
-------------------------------------------------------------------------------

> {-# LANGUAGE UnicodeSyntax #-}
>
> module Generate
> where
> -- import Unicode

< data Bool = False | True

> bools  ∷  [Bool]
> bools  =  pure False ++ pure True
>
> maybes  ∷  [elem] → [Maybe elem]
> maybes elems  =  pure Nothing ++ (pure Just <*> elems)

> data Suit = Spades | Hearts | Diamonds | Clubs              deriving (Show)
> data Rank = Faceless Integer | Jack | Queen | King | Ace    deriving (Show)
> data Card = Card Rank Suit | Joker                          deriving (Show)
>
> suits :: [Suit]
> suits = pure Spades ++ pure Hearts ++ pure Diamonds ++ pure Clubs
>
> ranks :: [Rank]
> ranks = (pure Faceless <*> [2..10]) ++ pure Jack ++ pure Queen ++ pure King ++ pure Ace
>
> cards :: [Card]
> cards = (pure Card <*> ranks <*> suits) ++ pure Joker

> data Tree elem = Empty | Node (Tree elem) elem (Tree elem)
>   deriving (Show)

< data List elem = [] | Cons elem (List elem)
< data List elem = [] | elem : (List elem)
< data List elems = [] | (:) elems (List elems)

> lists :: [elem] -> Integer -> [[elem]]
> lists _ 0        = pure []
> lists elems size = pure (:) <*> elems <*> (lists elems (size-1))

All trees with a certain depth

> trees :: [elem] -> Integer -> [Tree elem]
> trees _ 0     = pure Empty
> trees elems h = pure Node <*> (trees elems (h-1)) <*> elems <*> (trees elems (h-1))

All trees with a certain amount of elements


lists bools 1
lists bools 2
trees (lists bools 2) 1
trees (lists bools 2) 2
