import Data.List
import Tree
import Satellite
import Unicode

main = print (huffman relativeFrequencyTable)

-------------------------------------------------------------------------------
-- Exercise 1.1 : Frequency Table
-------------------------------------------------------------------------------

frequencyTable :: (Ord char) => [char] -> [With Int char]
frequencyTable input =
  let
    grouped = group (sort input)
    frequencies = (map length) grouped
    onlyCharacters = map head grouped
  in
    zipWith (:-) frequencies onlyCharacters

-------------------------------------------------------------------------------
-- Exercise 1.2 : Huffman Trees
-------------------------------------------------------------------------------

huffman :: [With Int Char] -> Tree Char
huffman = build . map (\ (freq :- letter) -> freq :- Leaf letter) . sort
  where
      build [a :- tree] =  tree
      build (a:b:xs) =
        let
          ab (freqA :- treeA) (freqB :- treeB)  = (freqA + freqB) :- (treeA :^: treeB)
        in
          build (insert (ab a b) xs)

relativeFrequencyTable :: [With Int Char]
relativeFrequencyTable =
    let
        -- We did everything times 1000, to convert them to Int
        rawTable = [('a', 8167),('b', 1492),('c', 2782),('d', 4253),('e', 12702),('f', 2228),('g', 2015),('h', 6094),('i', 6966),('j', 153),('k', 772),('l', 4025),('m', 2406),('n', 6749),('o', 7507),('p', 1929),('q', 95),('r', 5987),('s', 6327),('t', 9056),('u', 2758),('v', 978),('w', 2360),('x', 150),('y', 1974),('z', 74)]

        format (char, freq) =  freq :- char
    in
        map format rawTable

-------------------------------------------------------------------------------
-- Exercise 1.3 : ASCII-encoding
-------------------------------------------------------------------------------

data Bit = O | I
  deriving (Show, Eq, Ord)
-- TODO Hoe weet hij dat hij moet deriven van Booleans?

-- msg = message = the string that needs decoding
encode :: (Eq char) => Tree char -> [char] -> [Bit]
encode huffmanTree msg =
    [] {--
    let
    -- Note: encodedMsg is the reversed of msg
    encode' :: [char] -> [Bit] -> [Bit]
    encode' [] encodedMsg     = reverse encodedMsg
    encode' [m:ms] encodedMsg =
        encode' ms ((lookUp m dictionary) ++ encodedMsg)

    dictionary = code [] huffmanTree

    -- This could be faster, if the dictionary would generate a Huffman tree, rather than a plain list. Then we could BFS through it :-)
    -- dx is the remainder of the dictionary
    lookUp :: char -> [([Bit], char)] -> [Bit]
    lookUp letter [] = [] -- lookUp failed, semantically impossible, throw exception OSLT
    lookUp letter [(enc, char), dx] = -- dx :: ([Bit], char1), should be [a]
        if char == letter
            then enc
            else lookUp letter dx
    in
    encode' msg []
--}

-- The bitprefix is reversed, in order to append efficiently
-- I'm not satisfied with the ++ operator, it seems inefficient
-- This is the "codes" function, but has an longer/uglier definition
-- Given a tree, generates a dictionary
code :: [Bit] -> Tree char -> [([Bit], char)]
code bitPrefix charTree =
    case charTree of
        Leaf letter ->
            [(reverse bitPrefix, letter)]
        (:^:) left right ->
            (code (O:bitPrefix) left)
            ++ (code (I:bitPrefix) right)

-- TODO: I could refactor "code" into "codes", but first lemma finish ex 1.4
-- codes ∷ Tree char -> [(char, [Bit])]


-------------------------------------------------------------------------------
-- Exercise 1.4
-------------------------------------------------------------------------------
-- Why use 'char' instead of 'Char'? Enforce typing! :ggrrr:

decode :: Tree char -> [Bit] -> [char]
decode huffmanTree decodedString =
    []{--
    let
        dictionary = code [] huffmanTree
    in

--}

-------------------------------------------------------------------------------
-- Test data
-------------------------------------------------------------------------------

ezExample = "AAABBC"

why =
 "As software becomes more and more complex, it\n\
 \is  more  and  more important to structure it\n\
 \well.  Well-structured  software  is  easy to\n\
 \write,   easy   to   debug,  and  provides  a\n\
\collection  of modules that can be re-used to\n\
 \reduce future programming costs. Conventional\n\
 \languages place a conceptual limit on the way\n\
 \problems   can   be  modularised.  Functional\n\
 \languages  push  those  limits  back. In this\n\
 \paper we show that two features of functional\n\
 \languages    in    particular,   higher-order\n\
 \functions and lazy evaluation, can contribute\n\
 \greatly  to  modularity.  Since modularity is\n\
 \the key to successful programming, functional\n\
 \languages  are  vitally important to the real\n\
 \world."

-- Give string, does creepy stuff with it, returns logging of creepyness
finalTest :: String -> String
finalTest testString =
    let
        huffmanTree = huffman (frequencyTable testString)
        encodedString = encode huffmanTree testString
        decodedString = decode huffmanTree encodedString
        check = testString == decodedString
    in
        "HuffmanTree: " ++ show huffmanTree
        ++ "\nEncoded string: " ++ show encodedString
        ++ "\nDecoded string: " ++ show decodedString
        ++ "\nMy function works: " ++ show check
