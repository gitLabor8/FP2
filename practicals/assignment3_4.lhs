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
> cons e es        = newIORef (Cons e es)

Exercise 3.4.2

< fromList ∷ [elem] → IO (ListRef elem)
< toList   ∷ ListRef elem → IO [elem]

Exercise 3.4.3

< foreach ∷ ListRef a → (a → IO b) → IO (ListRef b)
