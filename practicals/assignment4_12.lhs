-------------------------------------------------------------------------------
-- Exercise 4.1 *and* 4.2
-- Dion Scheper      -- s4437578
-- Max van Laarhoven -- s4547136
-- Frank Gerlings    -- s4384873
-------------------------------------------------------------------------------

> {-# LANGUAGE UnicodeSyntax, TypeOperators #-}
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

> -- Default Applicative style
> evalA ∷ (Applicative f) ⇒ Expr → f Integer
> evalA (Lit i)      =  pure i
> evalA (e1 :+: e2)  =  pure (+)  <*> evalA e1 <*> evalA e2
> evalA (e1 :*: e2)  =  pure (*)  <*> evalA e1 <*> evalA e2
> evalA (Div e1 e2)  =  pure div  <*> evalA e1 <*> evalA e2

> toss  ∷  Expr
> toss  =  Lit 0 :?: Lit 1

> -- f = []
> evalN ∷ Expr → [Integer]
> evalN (Lit i)      =  pure i
> evalN (e1 :+: e2)  =  pure (+)  <*> evalN e1 <*> evalN e2
> evalN (e1 :*: e2)  =  pure (*)  <*> evalN e1 <*> evalN e2
> evalN (Div e1 e2)  =  pure div  <*> evalN e1 <*> evalN e2
> evalN (e1 :?: e2)  =  evalN e1 ++ evalN e2

evalN toss
evalN (toss :+: Lit 2 :*: toss)
evalN (toss :+: Lit 2 :*: (toss :+: Lit 2 :*: (toss :+: Lit 2 :*: toss)))

< evalR :: Expr -> f Integer
< evalR :: Expr -> Env -> Integer
< evalR :: Expr -> ((->) Env) Integer
< -- f = Env -> = (->) Env

> type Env = [(String, Integer)]

instance Applicative ((->) a) where
    -- pure i :: b -> i
    -- const  :: i -> b -> i
    pure = const
    (<*>) f g x = f x (g x)
    liftA2 q f g x = q (f x) (g x)

> evalR ∷ Expr → Env -> Integer
> evalR (Lit i)     = pure i
> evalR (e1 :+: e2) = pure (+) <*> evalR e1 <*> evalR e2
> evalR (e1 :*: e2) = pure (*) <*> evalR e1 <*> evalR e2
> evalR (Div e1 e2) = pure div <*> evalR e1 <*> evalR e2
> evalR (Var a)     = \env -> (case lookup a env of
>                               Nothing -> 0
>                               Just i  -> i)

evalR (Var "a" :+: Lit 1) [("a", 4711), ("b", 0815)]
evalR (Var "a" :*: Var "b") [("a", 4711), ("b", 0815)]
evalR (Var "a" :*: Var "c") [("a", 4711), ("b", 0815)]

Extra! Let's combine the two of these (Ralf also did so in the Werkcollege, so I've got to copy cat it)

Ok ok, I'm guilty. I used this as reference:
https://hackage.haskell.org/package/base-4.10.1.0/docs/Data-Functor-Compose.html

> newtype Comp f g a = Comp (f (g a))
> instance (Functor f, Functor g) => Functor (Comp f g) where
>   fmap h (Comp a) = Comp (fmap (fmap h) a)
>

Ignore this, I'm going back to usefull stuf again. Bai.

< instance (Applicative f, Applicative g) => Applicative (Comp f g) where
<   pure a      = pure (pure a)
< -- Compareable to: zipWith :: (a -> b -> c) -> [a] -> [b]
< -- zipWith h as bs                   =       (pure h)    as <*> bs
<   (<*>) (Comp h)           (Comp fg) = Comp ((pure (<*>)) h <*> fg)
< --      (Comp (a -> b)) -> (Comp g)

We will introduce "Comp", which yields a list of environments.
We choose to have multiple answers, and all can have a different environment
This is the one for a global environment

>-- evalX :: Expr -> Comp Int
>
>
