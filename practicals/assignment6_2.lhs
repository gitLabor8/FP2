> {-# LANGUAGE UnicodeSyntax #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE TypeFamilyDependencies #-}
>
> module Minimax2
> where
> -- import Unicode
> import Squiggol

Multiway trees.
-------------------------------------------------------------------------------
-- Exercise 6.1
-- Dion Scheper      -- s4437578
-- Max van Laarhoven -- s4547136
-- Frank Gerlings    -- s4384873
-------------------------------------------------------------------------------

> data Tree elem  =  Node elem [Tree elem]
>                                 deriving(Show)

> data TREE tree value = NODE tree [value]
>                                 deriving(Show)

> instance Functor (TREE tree) where
>     fmap f (NODE t vs) = NODE t (map f vs)

> instance Base (TREE tree) where
>    type Rec (TREE tree) = Tree tree
>    inn (NODE t vs) = Node t vs
>    out (Node e vs) = NODE e vs

> testtree = Node 5 [Node 3 [Node 5 []], Node 4 []]

unfold :: Base f => (a -> f a) -> a -> Rec f

> size, depth ∷ Tree elem → Integer
> size  = fold (\x -> case x of
>                       NODE t vs -> 1 + sum vs )
> depth = fold (\x -> case x of
>                       NODE t [] -> 1
>                       NODE t vs -> 1 + maximum vs)

> gametree ∷ (position → [position]) → (position → Tree position)
> gametree f = unfold (\p -> NODE p (f p))

> winning  ∷ Tree position → Bool

P has a winning strategy if any of his options is a winning strategy.
Thus ps should have at least 1 True, then the current position is True.

> winning = fold (\x -> case x of
>                         NODE p ps -> not (or ps))
