-------------------------------------------------------------------------------
-- Exercise 2.3
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

> data File = File { fileName :: String
>                  , lines :: Int
>                  , words :: Int
>                  , characters :: Int
>                  }
>   deriving (Show)

TODO: Redefine show into something prittyful
Do we need to account for * wildcards? (The example does :S )

> main :: IO ()
> main = do
>   (inputFilePath : []) <- getArgs
>   putStrLn ("inputfile = " ++ inputFilePath)
>   inputFileContent <- readFile inputFilePath
>   print (scanFile inputFilePath inputFileContent)

We want to read the filecontent inside scanfile. How to?

> scanFile :: String -> String -> File
> scanFile filePath file =
>   let
>      lines = length $ Data.String.lines file
>      words = length $ Data.String.words file
>      characters = length file
>   in
>       File filePath lines words characters
