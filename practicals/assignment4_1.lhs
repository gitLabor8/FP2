> {-# LANGUAGE UnicodeSyntax #-}
>
> module Evaluator
> where
> -- import Unicode

> infixl 6 :+:
> infixl 7 :*:
> infixr 1 :?:
>
> data Expr
>   =  Lit Integer    -- a literal
>   |  Expr :+: Expr  -- addition
>   |  Expr :*: Expr  -- multiplication
>   |  Div Expr Expr  -- integer division
>   |  Expr :?: Expr  -- non-deterministic choice
>   |  Var String     -- a variable

> evalA ∷ (Applicative f) ⇒ Expr → f Integer
> evalA (Lit i)      =  pure i
> evalA (e1 :+: e2)  =  pure (+)  <*> evalA e1 <*> evalA e2
> evalA (e1 :*: e2)  =  pure (*)  <*> evalA e1 <*> evalA e2
> evalA (Div e1 e2)  =  pure div  <*> evalA e1 <*> evalA e2
> evalA (Var s)      =  pure 0 -- a variable evaluates to zero

> toss  ∷  Expr
> toss  =  Lit 0 :?: Lit 1

> evalN ∷ Expr → [] Integer
> evalN (Lit i) = pure i
> evalN (e1 :+: e2) = pure (+) <*> evalN e1 <*> evalN e2
> evalN (e1 :*: e2) = pure (*) <*> evalN e1 <*> evalN e2
> evalN (Div e1 e2) = pure div <*> evalN e1 <*> evalN e2
> evalN (e1 :?: e2) = evalN e1 ++ evalN e2
> evalN (Var s)     = pure 0 -- a variable evaluates to zero

evalN toss
evalN (toss :+: Lit 2 :*: toss)
evalN (toss :+: Lit 2 :*: (toss :+: Lit 2 :*: (toss :+: Lit 2 :*: toss)))

> evalR ∷ Expr → [(String, Integer)] → Integer -- this is not correct yet
> evalR (Lit i) as    = pure i as
> evalR (e1 :+: e2) as = (+) (evalR e1 as) (evalR e2 as)
> evalR (e1 :*: e2) as = (*) (evalR e1 as) (evalR e2 as)
> evalR (Div e1 e2) as = div (evalR e1 as) (evalR e2 as)
> evalR (e1 :?: _)  as = evalR e1 as  -- dismiss right half, it is not deterministic

< evalR (Var s)     as = lookup s as

evalR (Var "a" :+: Lit 1) [("a", 4711), ("b", 0815)]
evalR (Var "a" :*: Var "b") [("a", 4711), ("b", 0815)]
evalR (Var "a" :*: Var "c") [("a", 4711), ("b", 0815)]
