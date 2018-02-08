ghc --make WordCount.lhs

> {-# LANGUAGE UnicodeSyntax #-}
>
> module Main
> where
> import Unicode ()
> import System.Environment

> main :: IO ()
> main = do
>   args <- getArgs
>   putStrLn (unwords args)
