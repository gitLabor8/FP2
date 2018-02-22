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


gametree ∷ (position → [position]) → (position → Tree position)

size ∷ Tree elem → Integer

winning  ∷ Tree position → Bool
losing   ∷ Tree position → Bool

evaluate ∷ Integer → Position → Value
evaluate depth  =  maximize static . prune depth . gametree moves

prune ∷ Integer → Tree elem → Tree elem

type Value = Int  -- |[-100 .. 100]|

static ∷ Position → Value

maximize  ∷ (position → Value) → (Tree position → Value)
minimize  ∷ (position → Value) → (Tree position → Value)
