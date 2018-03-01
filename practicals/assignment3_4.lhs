-------------------------------------------------------------------------------
-- Exercise 3.4
-- Dion Scheper      -- s4437578
-- Max van Laarhoven -- s4547136
-- Frank Gerlings    -- s4384873
-------------------------------------------------------------------------------

> {-# LANGUAGE UnicodeSyntax #-}
>
> module LinkedList
> where
> import Unicode
> import Data.IORef

> type ListRef elem  =  IORef (List elem)
>
> data List elem  =  Nil | Cons elem (ListRef elem)

Exercise 3.4.1

> nil  ∷ IO (ListRef elem)
> nil = newIORef Nil

> cons ∷ elem → ListRef elem → IO (ListRef elem)
> cons e es = newIORef (Cons e es)

Exercise 3.4.2

> fromList ∷ [elem] → IO (ListRef elem)
> fromList [] = newIORef Nil
> fromList (e : es) = newIORef $ Cons e (fromList es)

> toList   ∷ ListRef elem → IO [elem]
> -- Nu worden we weggejaagd door de portier
> -- Warning: Tijdens-Thalia-Borrel-code, not sure if BallmerPeaking. Pretty sure not BallmerPeaking though
> toList (IORef Nil)         = []
> toList (IORef (Cons a as)) = (a : (toList as))

Exercise 3.4.3

> foreach ∷ ListRef a → (a → IO b) → IO (ListRef b)
> foreach a op = map a op
>   where
>       map :: ListRef a → (a → IO b) → IO (ListRef b)
>       map (IORef Nil) op         = newIORef $ op []
>       map (IORef (Cons a as)) op = newIORef $ (Cons (op a) map as)
>
