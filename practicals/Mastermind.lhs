> {-# LANGUAGE UnicodeSyntax #-}
>
> module Mastermind
> where
> import Unicode ()
> import System.Random

> dice :: IO Int
> dice = getStdRandom (randomR (1,6))
>
> roll ∷ IO Int
> roll  =  do  a ← dice
>              b ← dice
>              return (a + b)
