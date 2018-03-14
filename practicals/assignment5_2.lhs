-------------------------------------------------------------------------------
-- Exercise 5.2
-- Dion Scheper      -- s4437578
-- Max van Laarhoven -- s4547136
-- Frank Gerlings    -- s4384873
-------------------------------------------------------------------------------

> {-# LANGUAGE UnicodeSyntax #-}
>
> module RandomAccessList12
> where
> -- import Unicode

< data Sequ elem
<
< nil   ∷ Sequ elem
< cons  ∷ elem → Sequ elem → Sequ elem
< head  ∷ Sequ elem → elem
< tail  ∷ Sequ elem → Sequ elem
< (!)   ∷ Sequ elem → Integer → elem
