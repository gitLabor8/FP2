import Data.List
import Tree

main = print (build (frequencyTable ezExample))

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

infix 1 :-
data With a b  =  a :- b
  deriving (Show)

satellite :: With a b -> b
satellite (_ :- b)  =  b

instance (Eq a) => Eq (With a b) where
  (a :- _) == (b :- _)  =  a == b
instance (Ord a) => Ord (With a b) where
  (a :- _) <= (b :- _)  =  a <= b
sndWith :: (With a b) -> b
sndWith (a :- b) = b

-- # Exercise 1.1

frequencyTable :: (Ord char) => [char] -> [With Int char]
frequencyTable input =
  let
    grouped = group (sort input)
    frequencies = (map length) grouped
    onlyCharacters = map head grouped
  in
    zipWith (:-) frequencies onlyCharacters

{--
DataType = (With a b)
a = Frequency
b = letter
c = With a b
--}

-- Exercise 1.2

--data Tree a = (:^:) (Tree a) (Tree a)
--       | Leaf a

{--/begin haskell comment
Tree a = Node (Tree a) (Tree a)
| Leaf a

case Tree a of
  Node t1 t2 ->
    magix t1 t2
  Leaf a ->
    magix a

Geheime constructors
--------------------
Node :: Tree a -> Tree a -> Tree a
Leaf :: a -> Tree a

eg: (:^:)
      (Leaf 4000)
      (Leaf 1337)

// geven we onze freq table
// levert ons een balanced tree
buildBalanced :: [elem] -> Tree elem
buildBalanced [a] = Leaf a
buildBalanced as  = build (take n as) :^: build (drop n as)
  where n = length as ('div' 2)

/einde haskell comment
--}


build :: [With Int b] -> Tree b
build = build' . map (\ (freq :- letter) -> freq :- Leaf letter) . sort
  where
--      build' :: [With a (Tree b)] -> Tree b
      build' [a :- tree] =  tree
      build' (a:b:xs) =
        let
          ab (freqA :- treeA) (freqB :- treeB)  = (freqA + freqB) :- (treeA :^: treeB)
        in
          build' (insert (ab a b) xs)

  -- let
  --   sortedFrequencyTable = sort frequencyTable
  --   singleTrees =  map (\ (freq :- letter) -> freq :- Leaf letter) sortedFrequencyTable
  --
  --   a:b:xs = singleTrees
  --   ab (With freqA treeA) (With freqB treeB)  = With (freqA + freqB)(Node treeA treeB)
  --   -- Maybe a bit inefficient. Oh well...
  --   sort ((ab a b):xs)
  -- in
