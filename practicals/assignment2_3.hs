-------------------------------------------------------------------------------
-- Exercise 2.3
-- Dion Scheper      -- s4437578
-- Max van Laarhoven -- s4547136
-- Frank Gerlings    -- s4384873
-------------------------------------------------------------------------------


{-# LANGUAGE UnicodeSyntax #-}

module Stream
where
import Prelude hiding (head, tail, repeat, map, zip, take, sum)
import Unicode

data Stream elem  =  Cons { head :: elem, tail :: Stream elem }

--instance Show :: Show elem => Stream elem -> IO ()

infixr 5 ≺
(≺)    ∷  elem → Stream elem → Stream elem
a ≺ s  =   Cons a s

from ∷ Integer → Stream Integer
from n = n ≺ from (n + 1)

-- 1 Make lift functions -- Do you even lift bro?
repeat  ∷  a → Stream a
repeat a = Cons  a (repeat a)

map     ∷  (a → b) → Stream a → Stream b
map f (Cons a as)
     = Cons (f a) (map f as)

zip     ∷  (a → b → c) → Stream a → Stream b → Stream  c
zip f (Cons a as) (Cons b bs) = Cons (f a b) (zip f as bs)

-- 2 Make stream an instance of Num -- 404 Joke not found
instance (Num elem) => Num (Stream elem) where
    (+) as bs = zip (+) as bs
    (-) as bs = zip (-) as bs
    (*) as bs = zip (*) as bs

    fromInteger n = repeat (fromInteger n)
    signum as     = map signum as
    negate as     = map negate as
    abs as        = map abs as

nat, fib ∷ Stream Integer
nat  =  0 ≺ nat + 1
fib  =  0 ≺ 1 ≺ fib + tail fib

-- 3 Taaaaake ooooon meeeeeeee, take on me!
take ∷ Integer → Stream elem → [elem]
take 0 _           = []
take n (Cons a as) = a : (take (n-1) as)

instance (Show elem) => Show (Stream elem) where
    show s = show (take 20 s)

-- 4 Hilary Diff
diff :: (Num elem) => Stream elem -> Stream elem
diff s  = abs (tail s - s)

diffAlt :: (Num elem) => Stream elem -> Stream elem
diffAlt (Cons a as) = abs (a - (head as)) ≺ diffAlt as

sum :: (Num elem) => Stream elem -> Stream elem
sum s = 0 ≺ s + (sum s)

-- We needed to derive these solutions. We did not.
-- Luckily, Ward-senpai gave us the answers:
{--
diff s = tail s - s
diff (sum s) = tail (sum s) - sum s
s = tail (sum s) - sum s
s + sum s = tail (sum s)
Cons (head (sum s)) (s + sum s) = Cons (head (sum s))  (tail (sum s))
Cons (head (sum s)) (s + sum s) = sum s
sum s = Cons 0 (s + sum s)
--}
