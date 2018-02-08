ghc --make OTP.lhs


> {-# LANGUAGE UnicodeSyntax #-}
>
> module Main
> where
> import Unicode ()
> import System.Random
> import System.Environment

> dice :: IO Int
> dice = getStdRandom (randomR (1,6))
>
> roll ∷ IO Int
> roll  =  do  a ← dice
>              b ← dice
>              return (a + b)

> main :: IO ()
> main = do
>   setStdGen (mkStdGen 4711)
>   (arg : _) <- getArgs
>   let n = read arg :: Int
>   rs <- sequence (replicate n roll)
>   print rs
>   putStrLn ("average: " ++ show (sum rs `div` n))
