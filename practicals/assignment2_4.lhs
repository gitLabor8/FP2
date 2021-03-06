-------------------------------------------------------------------------------
-- Exercise 2.4
-- Dion Scheper      -- s4437578
-- Max van Laarhoven -- s4547136
-- Frank Gerlings    -- s4384873
-------------------------------------------------------------------------------

> {-# LANGUAGE UnicodeSyntax #-}
> module Assignment2_4
> where
> import Unicode

1. Calculate ALL THE POSITIONS

> type Position = (Integer, Integer)

> moves :: Position -> [Position]
> moves (a,b) = [(x,y) | x <- [1..(max a b)], y <- [1..(max a b)], x <= y, (x+y == a) || x + y == b]

Feedback: This could be more efficient:
Why would you need to iterate y over all possible values between 1 and the max of a and b? There are only 2 possible values for y for every given value of x (a-x and b-x).

2 Binary trees are for losers

> data Tree elem  =  Node elem [Tree elem]
>
> test = Node 1 [Node 0 [Node 3 []], Node 2 []]
>
> instance (Show elem) => Show (Tree elem) where
>   show t = (show' 0 t) where
>       show' :: (Show elem) => Int -> (Tree elem) -> String
>       show' indent (Node elem []) =
>           (fancyIndent indent) ++ (show elem) ++ "\r\n"
>       show' indent (Node elem tx) =
>           (fancyIndent indent) ++ (show elem) ++ "\r\n"
>           ++ concat (map (show' (indent + 1)) tx)
>
>       fancyIndent :: Int -> String
>       fancyIndent 0 = ""
>       fancyIndent 1 = "+-"
>       fancyIndent n = (fancyIndent (n-1)) ++ "--"
>


> gametree ∷ (a → [a]) → a → Tree a
> gametree moves a
>                   |  length (moves a) == 0 = Node a []
>                   |  otherwise             = Node a [gametree moves x | x <- (moves a)]


> size ∷ Tree elem → Integer
> size (Node _ [])     = 1
> size (Node _ as)     = 1 + foldr (+) 0 (map size as)

Feedback:
Your base case ([]) is unnecessary here, since the case for as gives the same result as that case if as=[].

> winning  ∷ Tree position → Bool
> winning (Node _ as) = foldr (||) False (map losing as)

> losing   ∷ Tree position → Bool
> losing (Node _ as) = foldr (&&) True (map winning as)


evaluate ∷ Integer → Position → Value
evaluate depth  =  maximize static . prune depth . gametree moves

prune cuts off all elements below the given depth

> prune ∷ Integer → Tree elem → Tree elem
> -- This should only happen when someone immediatly requests a tree of depth 0
> prune 0 t              = t
> -- Here we cut off the tree
> prune 1 (Node elem ax) = Node elem []
> prune n (Node elem ax) = Node elem ((map (prune (n-1))) ax)

> type Value = Int  -- |[-100 .. 100]|

> getElem :: Tree elem -> elem
> getElem (Node a _) = a

> average :: [Int] -> Int
> average as = (div (sum (map fromIntegral as)) (length as)) :: Int

> static ∷ Position → Value
> static (1,1) = -100
> static pos = negate (average (map static (map getElem trees)))
>            where (Node _ trees) = gametree moves pos

 Dual recursion functions, cool!

> maximize  ∷ (position → Value) → (Tree position → Value)
> maximize eval (Node pos []) = eval pos
> maximize eval (Node pos ax) = maximum (map (minimize eval) ax)

> minimize  ∷ (position → Value) → (Tree position → Value)
> minimize eval (Node pos []) = negate (eval pos)
> minimize eval (Node pos ax) = minimum (map (maximize eval) ax)
