-------------------------------------------------------------------------------
-- Exercise 4.1 *and* 4.2
-- Dion Scheper      -- s4437578
-- Max van Laarhoven -- s4547136
-- Frank Gerlings    -- s4384873
-------------------------------------------------------------------------------

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
>   deriving (Show)

> evalA ∷ (Applicative f) ⇒ Expr → f Integer
> evalA (Lit i)      =  pure i
> evalA (e1 :+: e2)  =  pure (+)  <*> evalA e1 <*> evalA e2
> evalA (e1 :*: e2)  =  pure (*)  <*> evalA e1 <*> evalA e2
> evalA (Div e1 e2)  =  pure div  <*> evalA e1 <*> evalA e2

> toss  ∷  Expr
> toss  =  Lit 0 :?: Lit 1

> evalN ∷ Expr → [Integer]
> evalN (Lit i)      =  pure i
> evalN (e1 :+: e2)  =  pure (+)  <*> evalN e1 <*> evalN e2
> evalN (e1 :*: e2)  =  pure (*)  <*> evalN e1 <*> evalN e2
> evalN (Div e1 e2)  =  pure div  <*> evalN e1 <*> evalN e2
> evalN (e1 :?: e2)  =  evalN e1 ++ evalN e2

evalN toss
evalN (toss :+: Lit 2 :*: toss)
evalN (toss :+: Lit 2 :*: (toss :+: Lit 2 :*: (toss :+: Lit 2 :*: toss)))

> type Env = [(String, Integer)]

> evalR ∷ Expr → Env → Integer
> evalR (Lit i) _     =  i
> evalR (e1 :+: e2) env = pure (+)  <*> evalR e1 env <*> evalR e2 env
> evalR (e1 :*: e2) env = pure (*)  <*> evalR e1 env <*> evalR e2 env
> evalR (Div e1 e2) env = pure div  <*> evalR e1 env <*> evalR e2 env
> evalR (Var a) env     = pure (case lookup a env of
>                               Nothing -> 0
>                               Just i  -> i)

evalR (Var "a" :+: Lit 1) [("a", 4711), ("b", 0815)]
evalR (Var "a" :*: Var "b") [("a", 4711), ("b", 0815)]
evalR (Var "a" :*: Var "c") [("a", 4711), ("b", 0815)]
