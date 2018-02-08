> {-# LANGUAGE UnicodeSyntax #-}
> {-# LANGUAGE TypeFamilies #-}
>
> module Minimax2
> where
> -- import Unicode
> import Squiggol

Multiway trees.

> data Tree elem  =  Node elem [Tree elem]

< size, depth ∷ Tree elem → Integer

< gametree ∷ (position → [position]) → (position → Tree position)

< winning  ∷ Tree position → Bool
