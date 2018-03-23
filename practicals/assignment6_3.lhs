> {-# LANGUAGE UnicodeSyntax #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE TypeFamilies #-}
>
> module Sorting
> where
> import Unicode
> import Squiggol

> data Tree elem  =  Empty | Node (Tree elem) elem (Tree elem)

> data TREE elem tree  =  EMPTY | NODE tree elem tree

> instance Functor (TREE elem) where
>   fmap _f  (EMPTY)       =  EMPTY
>   fmap f   (NODE l a r)  =  NODE (f l) a (f r)

> instance Base (TREE elem) where
>   type Rec (TREE elem) = Tree elem
>   inn (EMPTY)       =  Empty
>   inn (NODE l a r)  =  Node l a r
>   out (Empty)       =  EMPTY
>   out (Node l a r)  =  NODE l a r

Growing a search tree.

> grow1, grow2 ∷ (Ord elem) ⇒ [elem] -> Tree elem
> grow1  =  unfold  (para  (fmap (id ▿ inn) ∘ sprout))
> grow2  =  fold    (apo   (sprout ∘ fmap (id ▵ out)))

> sprout ∷ (Ord a) ⇒ List a (x × TREE a x) → TREE a (x + List a x)
> sprout = undefined

Flattening a search tree.

> flatten1, flatten2 ∷ (Ord elem) ⇒ Tree elem -> [elem]
> flatten1  =  fold    (apo   (wither ∘ fmap (id ▵ out)))
> flatten2  =  unfold  (para  (fmap (id ▿ inn) ∘ wither))

> wither ∷ (Ord a) ⇒ TREE a (x × List a x) → List a (x + TREE a x)
> wither = undefined
