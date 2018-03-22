-------------------------------------------------------------------------------
-- Exercise 5.2
-- Dion Scheper      -- s4437578
-- Max van Laarhoven -- s4547136
-- Frank Gerlings    -- s4384873
-------------------------------------------------------------------------------

Welcome to our neat backlog for proving in Functional Programming 2, using execercise 6.4 as example. Please enjoy the structure and verbosity.

For repetition, the functor laws:
<    fmap id = id                                       (1a)
< fmap (f.g) = fmap f . fmap g                          (1b)

Our assumption back in excercise 4.4: (don't use this in 6.4)
< fmap f m = pure f <*> m                               (assumption)

The applicative functor laws:
<              pure id <*> v = v                        (2a)
< pure (.) <*> u <*> v <*> w = u <*> (v <*> w)          (2b)
<          pure f <*> pure x = pure (f x)               (2c)
<               u <*> pure x = pure (\f -> f x) <*> u   (2d)

Random property of execercise 4.4.2:
< pure f <*> u <*> pure x = pure (flip f) <*> pure x <*> u  (3)

< fold :: (Base f) => (f a -> a) -> (Rec f -> a)
< fold a = a . fmap (fold a) . out

The uniqueness property:
< f = fold a <=> f . inn = a . fmap f           (4)

1. Show that the uniqueness property (4) implies the computation law (5):
< fold b . inn = a . fmap (fold b)              (5)

Let's commence:
< f = fold a <=> f . inn = a . fmap f           (4)
<<< choose "f = fold b" and "a = b"
< fold b = fold b
<   <=> (fold b) . inn = b . fmap (fold b)
<<< thus
< true
<   <=> (fold b) . inn = b . fmap (fold b)
<<< this already ends our proof, since:
> true => (fold b) . inn = b . fmap (fold b)

2. Show that the uniqueness property implies the reflection law (6):
< id = fold inn                                 (6)
<<< Note: inn is an algebra (I don't know how this is useful though...)
< inn :: f (Ref f) -> Rec

Let's start with the
< f = fold a
<   <=> f . inn = a . fmap f           (4)
<<< Choose "f = id" and "a = inn"
< id = fold inn
<   <=> id . inn = inn . fmap id
<<< Use the functor law (1a) "fmap id = id"
< id = fold inn
<   <=> id . inn = inn . id
<<< Now we use the left and right identity of functions
< id = fold inn
<   <=> inn = inn
<<< Now: "inn = inn" is true
< id = fold inn
<   <=> true
<<< And so we are done with our proof:
< id = fold inn    (6)
<   <= true

3. Finally, show that the uniqueness property implies the fusion law (7):
< h . fold c = fold d
<   <= h . c = d . fmap h     (7)
The fusion law states a condition for fusing a function with a fold to form another fold.

Let's start again with our uniqueness property:
< f = fold a
<   <=> f . inn = a . fmap f    (4)
<<< Now substitute "f = h . fold c" and "a = d"
< h . fold c = fold d
<   <=> h . fold c . inn = d . fmap (h fold c)
<<< Use (5): "fold c . inn = c . fmap (fold c)"
< h . fold c = fold d
<   <=> h . c . fmap (fold c) = d . fmap (h fold c)
<<< Now use one of the functor laws (1b) "fmap (h (fold c)) = fmap h . fmap (fold c)"
< h . fold c = fold d
<   <=> h . c . fmap (fold c) = d . fmap h . fmap (fold c)
<<< Now we drop the "fmap (fold c)" at the right
< h . fold c = fold d
<   <= h . c = d . fmap h
<<< Which is the fusion law (7), so we're done :-)

4. Use the properties above to show that inn and fold (fmap inn) are inverses of each other:
< fold (fmap inn) . inn = id                (X)
< inn . fold (fmap inn) = id                (Y)
In other words:
< fold (fmap inn) = out.

It's a trap! First proof (Y), then prove (X)!
We start with (Y) and try to create a valid law
< inn . fold (fmap inn) = id    (Y)
<<< Now use the reflection law (6)
< inn . fold (fmap inn) = fold inn
<<< Use the fusion law (note that the above statement is implied by the one below, but not the other way around)
<<< "h = inn", "a = fmap inn" and "b = inn"
< inn . (fmap inn) = inn . (fmap inn)
<<< Now that the left side and the right side are equal, we have reached a state

Lastly, we prove (X). Use
< fold (fmap inn) . inn = id                (X)
<<< Use the computation law (5), with "a = fmap inn"
< (fmap inn) . fmap (fold (fmap inn)) = id
<<< Now use one of the functor laws (1b)
< fmap (inn . fold (fmap inn)) = id
<<< Now we can use (Y) (do you see, it was a trap!)
< fmap id = id
<<< Which is equal to the first functor law (1a). Therefore the theorem is now proven!
