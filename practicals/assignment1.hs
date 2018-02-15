import Data.List
import Tree
import Satellite
import Unicode

main = putStrLn (finalTest randomNumbers)

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

huffman :: [With Int char] -> Tree char
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

-- msg = message = the string that needs decoding
encode :: (Eq char) => Tree char -> [char] -> [Bit]
encode huffmanTree msg =
--    [] {-- (Un)comment for default function
    let
    -- Note: encodedMsg is the reversed of msg
    -- GHCI can't match this function definition with the main one, because 'char' is polymorf :-(
    -- encode' :: (Eq char) => [char] -> [Bit] -> [Bit]
    encode' [] encodedMsg     = reverse encodedMsg
    encode' (m:ms) encodedMsg =
        encode' ms ((lookUp m dictionary) ++ encodedMsg)

    dictionary = codes huffmanTree

    -- This could be faster, if the dictionary would generate a Huffman tree, rather than a plain list. Then we could BFS through it :-)
    -- dx is the remainder of the dictionary
    lookUp :: (Eq char) => char -> [(char, [Bit])] -> [Bit]
    lookUp letter [] = [] -- lookUp failed, semantically impossible, throw exception OSLT
    lookUp letter ((char, enc): dx) =
        if char == letter
            then (reverse enc) -- The encryption string is reversed, for optimal 'appending'
            else lookUp letter dx
    in
    encode' msg []
--}

codes :: Tree char -> [(char, [Bit])]
codes huffmanTree =
    let
        -- The bitprefix is reversed, in order to 'append' efficiently
        -- I'm not satisfied with the ++ operator, it seems inefficient
        -- This is the "codes" function, but has an longer/uglier definition
        -- Given a tree, generates a dictionary
        codes' :: [Bit] -> Tree char -> [(char, [Bit])]
        codes' bitPrefix charTree =
            case charTree of
                Leaf letter ->
                    [(letter, reverse bitPrefix)]
                (:^:) left right ->
                    (codes' (O:bitPrefix) left)
                    ++ (codes' (I:bitPrefix) right)
    in
        codes' [] huffmanTree

-------------------------------------------------------------------------------
-- Exercise 1.4
-------------------------------------------------------------------------------

-- Gets a huffmantree and an encoded string
decode :: Tree char -> [Bit] -> [char]
decode ogHuffmanTree encMsg =
    --[]{-- (Un)comment for default function
    let
        --Again, polymorfic 'char' cannot be infered (or something like that)
        --decode' :: Tree char -> [Bit] -> [char]
        decode' (Leaf letter)    []     = [letter]
        decode' (left :^: right) []     = [] -- A half-read word at the end will be chopped off #notMyProblem. Shouldn't semantically happen
        decode' (Leaf letter)    encMsg =
            (letter : (decode' ogHuffmanTree encMsg))
        decode' (left :^: right) (O:bx) = decode' left bx
        decode' (left :^: right) (I:bx) = decode' right bx
    in
        decode' ogHuffmanTree encMsg
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

-- This works on other lists too. Awesome!
randomNumbers = [0,1,2,4,1,0,1]

-- Given something, does creepy stuff with it, returns logging of creepyness
finalTest :: (Show char) => (Eq char) => (Ord char) => [char] -> String
finalTest testString =
    let
        huffmanTree = huffman (frequencyTable testString)
        encodedString = encode huffmanTree testString
        decodedString = decode huffmanTree encodedString
        check = testString == decodedString
    in
        "Original string:   " ++ show testString
        ++ "\r\nHuffmanTree:       " ++ show huffmanTree
        ++ "\r\nEncoded string:    " ++ show encodedString
        ++ "\r\nDecoded string:    " ++ show decodedString
        ++ "\r\nMy function works: " ++ show check
