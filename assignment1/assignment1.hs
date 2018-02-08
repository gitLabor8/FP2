import Data.List

main = print (frequencyTable why)

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
{--
# Exercise 1.2
/begin haskell comment
Tree a = Node (Tree a) (Tree a)
       | Leaf a
Tree a = (:^:) (Tree a) (Tree a)
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

* sort [with a b]
*
build :: [With a b] -> [with a (Tree b)]


*
--}
