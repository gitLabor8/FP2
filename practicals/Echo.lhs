ghc --make Echo.lhs

> module Main where
> import System.Environment
>
> main :: IO ()
> main = do  args <- getArgs
>            putStrLn (unwords args)
