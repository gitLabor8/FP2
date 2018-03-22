> {-# LANGUAGE UnicodeSyntax #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE TypeFamilyDependencies #-}

> module DigitalSearching
> where
> import Prelude hiding (lookup)
> import Data.Maybe
> -- import Unicode

-------------------------------------------------------------------------------
-- Exercise 5.3
-- Dion Scheper      -- s4437578
-- Max van Laarhoven -- s4547136
-- Frank Gerlings    -- s4384873
-------------------------------------------------------------------------------

> data family Map key ∷ * → *
>
> class (Ord key) ⇒ Key key where
>   empty   ∷  Map key val
>   insert  ∷  key → (Maybe val → val) → Map key val → Map key val
>   lookup  ∷  key → Map key val → Maybe val

> data instance Map ()                 val  =  Empty | Single val deriving(Show, Eq)
>
> instance Key () where
>   empty  =  Empty
>   insert () f (Empty)     =  Single (f Nothing)
>   insert () f (Single v)  =  Single (f (Just v))
>   lookup () (Empty)     =  Nothing
>   lookup () (Single v)  =  Just v

> data instance Map (Either key1 key2) val  = MaxMap (Map key1 val) (Map key2 val)

> instance (Key key1, Key key2) => Key (Either key1 key2) where
>   empty = MaxMap empty empty
>   insert (Left a)  f (MaxMap x y) = MaxMap (insert a f x) y
>   insert (Right b) f (MaxMap x y) = MaxMap x (insert b f y)
>   lookup (Left a)  (MaxMap x y) = lookup a x
>   lookup (Right b) (MaxMap x y) = lookup b y

> data instance Map (key1, key2) val  = DionMap (Map key1 val, Map key2 val)

> instance (Key key1, Key key2) => Key (key1, key2) where
>   empty = DionMap (empty, empty)
>   insert (k1, k2) f (DionMap (x, y)) = DionMap (insert k1 f x, insert k2 f y)
>   lookup (x,y) (DionMap (m1, m2)) = lookup x m1

<                              | (==) (Just x) (lookup x m1) = lookup y m2   -- or lookup y m2??
<                              | otherwise                  = lookup x m1

> type List elem  =  Either () (elem, [elem])
>
> toList ∷ [elem] → List elem
> toList []        =  Left ()
> toList (a : as)  =  Right (a, as)

> data instance Map [key] val  = FrankMap (Map (List key) val)
>

> instance (Key key) ⇒ Key [key] where
>    empty = FrankMap empty
>    insert lk f (FrankMap m) = FrankMap (insert (toList lk) f m)
>    lookup lk (FrankMap m) = lookup (toList lk) m
