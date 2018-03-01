-------------------------------------------------------------------------------
-- Exercise 3.1
-- Dion Scheper      -- s4437578
-- Max van Laarhoven -- s4547136
-- Frank Gerlings    -- s4384873
-------------------------------------------------------------------------------


> {-# LANGUAGE UnicodeSyntax #-}
>
> module Main
> where
> import Unicode ()
> import System.Environment
> import Data.String
> import Data.Char
> import Data.Traversable
> import Control.Monad

> data File = File { fileName :: String
>                  , lines :: Int
>                  , words :: Int
>                  , characters :: Int
>                  }
>   deriving (Show)

TODO: Redefine show into something prittyful

> main :: IO ()
> main = do
>   inputFilePaths <- getArgs
>   filecontents <- for inputFilePaths readFile
>   print (zipWith scanFile inputFilePaths filecontents)

We want to read the filecontent inside scanfile. How to?

> scanFile :: String -> String -> File
> scanFile filePath filecontent =
>   let
>      lines = length $ Data.String.lines filecontent
>      words = length $ Data.String.words filecontent
>      characters = length filecontent
>   in
>       File filePath lines words characters
