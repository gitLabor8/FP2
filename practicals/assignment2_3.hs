{-# LANGUAGE UnicodeSyntax #-}

module Stream
where
import Prelude hiding (head, tail, repeat, map, zip, take, sum)
import Unicode

data Stream elem  =  Cons { head ∷ elem, tail ∷ Stream elem }

infixr 5 ≺
(≺)    ∷  elem → Stream elem → Stream elem
a ≺ s  =   Cons a s

from ∷ Integer → Stream Integer
from n = n ≺ from (n + 1)

-- 1 Make lift functions
repeat  ∷  a → Stream a
repeat a = Cons { head :: a, tail :: repeat a}

map     ∷  (a → b) → Stream a → Stream b
map f (Cons a as)
     = Cons (f a) (map f as)
--     = f a <<< map f as

zip     ∷  (a → b → c) → Stream a → Stream b → Stream  c
zip f (Cons a as) (Cons b bs) = Cons (f a b) (zip f as bs)

-- 2 Make stream an instance of Num
instance (Num elem) ⇒ Num (Stream elem) where
    (+) as bs = zip (+) as bs
    (-) as bs = zip (-) as bs
    (*) as bs = zip (*) as bs

    negate as = map negate as
    abs as    = map abs as

nat, fib ∷ Stream Integer
nat  =  0 ≺ nat + 1
fib  =  0 ≺ 1 ≺ fib + tail fib

-- 3 Taaaaake ooooon meeeeeeee, take on me!
take ∷ Integer → Stream elem → [elem]
take 0 _ = []
take n (Cons a as) = a (take (n-1) as)

-- 4 Hilary Diff
diff :: (Num elem) => Stream elem -> Stream elem
-- Given, but true?
--diff s  =  tail s - s
diff xs = map diff' xs
    where
        diff' (Cons a as) = abs (a - (head as)

diffAlt :: (Num elem) => Stream elem -> Stream elem
diffAlt (Cons a as) = abs (a - (head as)) ≺ diffAlt as
--diffAlt (Cons a as) = abs (a - (head as)) ≺ diffAlt as

sum :: (Num elem) => Stream elem -> Stream elem
sum (Cons a as) = 0 ≺ fib + sum
