-------------------------------------------------------------------------------
-- Exercise 3.2
-- Dion Scheper      -- s4437578
-- Max van Laarhoven -- s4547136
-- Frank Gerlings    -- s4384873
-------------------------------------------------------------------------------

> {-# LANGUAGE UnicodeSyntax #-}
>
> module Mastermind
> where
> import Unicode ()
> import System.Random
> import Data.List

Our beloved datatypes.
You can add colors and our program will scale with it :rainbow:
PS

> data Color = Black | White
>   deriving (Show, Eq, Bounded, Enum, Read)
> instance Random Color where
>   random g = case randomR (fromEnum (minBound :: Color), fromEnum (maxBound :: Color)) g of
>       (r, g') -> (toEnum r, g')
>   randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
>       (r, g') -> (toEnum r, g')
>
> type Code = [Color]

Shows how many colors are correctly colored and placed

> correct :: Code -> Code -> Int
> correct (c : cs) (d : ds)
>  | c == d     = 1 + correct cs ds
>  | otherwise  = correct cs ds
> -- Stops the compiler from complaining that one code could be longer than the other
> correct _ _ = 0

Shows how many colors are right, but are wrongly placed

> -- Given an guess cs and a solution ds, computes how many colors are right, but possibly wrongly placed
> contains :: Code -> Code -> Int
> contains cs ds = (length ds) - (length difference)
>   where
>       difference = foldr delete ds cs

first heuristic: correct
second heuristic: contains - correct

> main :: IO ()
> main = do
>   setStdGen $ mkStdGen (42*10)
>   firstSeed <- getStdGen
>   putStrLn "Hallo! We spelen met deze kleuren:"
>   print [(minBound :: Color) .. (maxBound :: Color)]
>   putStrLn "Hoelang moet je code worden?"
>   codeLength <- fmap read getLine
>   randomColor <- pure $ randoms firstSeed  :: IO Code
>   secretCode <- pure $ take codeLength randomColor
>   guess secretCode codeLength 12
>
> guess :: Code -> Int -> Int -> IO()
> guess secretCode codeLength 0 = print "You failed! NOooOOOoOOOoo" --b
> guess secretCode codeLength n = do
>   print $ "You've got " ++ show n ++ " chances left..."
>   print "Please try a combination"
>   try <- fmap read getLine
>   (rightPos, wrongPos) <- pure $ sitRep try secretCode
>   print $ "You've got " ++ show rightPos ++ " correct colours on the correct position and " ++ show wrongPos ++ " correct colours on the wrong position."
>   if codeLength == rightPos then
>        print "Jij. Jij bent gewoon een winnaar!\nEen geweldige triomphaire. Eur. Zeg een triomterreurfereurrer. Ok, je bent gewoon een toffe peer. Où le Père"
>   else
>        guess secretCode codeLength (n-1)
>
> sitRep :: Code -> Code -> (Int, Int)
> sitRep try secret = (rightPos, wrongPos)
>   where
>       rightPos = correct try secret
>       wrongPos = (contains try secret) - (correct try secret)

Test examples for in the interpreter

> allBlack = [Black, Black, Black]
> allWhite = [White, White, White]
> mixed    = [Black, White, Black]
> mixedRev = [White, Black, White]
