-----------
title: 'Day 1: Report Repair'
subtitle: 'Advent of Code 2020'
-----------

> module Day1 (run1, run2) where
> import Relude.Extra.Foldable1
> import Data.Bits hiding (rotate)
> --import Relude as Prelude
> --import qualified Prelude as Prelude
> import qualified Data.Map.Lazy  as LazyMap
> import qualified Data.IntMap.Strict as StrictIntMap
> import qualified Data.Set as Set

 <!--imports-->

Challenge 1
===

The input is a simple list of numbers, up to 4 digits long:
    234
    4823
    1234
    ...

And we have to find the two numbers which add to 2020, and then multiply them.

Naive Approach
------

It's day 1 and the input is small, so let's start with a naive combinatoric approach.
Starting off, we need to get all the length 2 subsequences in the list.

The default Applicative instance for Lists is a good start, as applies a list of functions over
the *product* of all arguments.

``
(<*>) @([]) :: [a -> b] -> [a] -> [b]
``

> combinations2 :: [a] -> [(a,a)]
> combinations2 xs = (,) <$> xs <*> xs

However, this is not what we want... each element is also paired with itself.

We need to pair each element with the rest of the list, this is much trickier.
For each element, we can rotate the list such that it appears at the front.

Here's this seemingly magic rotate function I came across on twitter, which (ab?)uses the
instance `Semigroup b => Semigroup (a -> b)`.

> rotate :: Int -> [a] -> [a]
> rotate = (drop <> take)

Now, getting all rotations of the list:

> rotations :: [a] -> [[a]]
> rotations xs =
>   rotate <$>
>       [0..(length xs - 1)] <*> (pure xs)

We can now get the sequences by splitting each of the rotations at the head,
then combining it with each element in the tail

> sequences2 :: [a] -> [(a,a)]
> sequences2 =
>   rotations
>     >>> (>>= (splitAt 1) >>> (uncurry $ liftA2 (,)))

Now let's actually write the search. We need 2 values which add up to 2020, which we will multiply at the end/

> run1 :: [String] -> Maybe (Int)
> run1 =
>   (mapMaybe (readMaybe @(Int)))
>     >>> sequences2
>     >>> find ((uncurry (+)) >>> (==2020))
>     >>> (fmap $ uncurry (*))
>

Challenge 2
===

The second challenge is to do the same, but for subsequences of 3 numbers...
We can generalise sequences2 to sequencesN:

This is going to be tricky, so I'm going with a different approach.
We can get a subsequence from a list by taking the value at many unique indices, or as I'm going to think of it,
the list of unique indices is the 'index' of a subsequence.

We could encode this as a list of integers, but that would allow for duplicate indices. Instead we can use a one-hot encoding
as the index!

> indexOneHot :: [a] -> [Bool] -> [a]
> indexOneHot []       _             = []
> indexOneHot _        []            = []
> indexOneHot (x : xs) (True  : ixs) = x : (indexOneHot xs ixs)
> indexOneHot (_ : xs) (False : ixs) = indexOneHot xs ixs

> intsToOneHot :: [Int] -> [Bool]
> intsToOneHot xs = go 0 $ sort xs
>   where
>     go :: Int -> [Int] -> [Bool]
>     go _ []     = []
>     go n xs@(x:next) | x == n    = True  : go (n+1) next
>                      | otherwise = False : go (n+1) xs

Generating the sequence indices by first choosing a single index, and then
afterwards only choosing lower indices.

> indicesInt :: Int -> Int -> [[Int]]
> indicesInt 0 n = []
> indicesInt 1 n = [[y] | y <- [0..n]]
> indicesInt i n = [(x : ys) | x <- [0..n], ys <- indicesInt (i-1) (x-1) ]

Gluing it all together:

> indicesOneHot :: Int -> Int -> [[Bool]]
> indicesOneHot n i = intsToOneHot <$> (indicesInt i n)
>
> subsequencesN :: Int -> [a] -> [[a]]
> subsequencesN n xs = fmap (indexOneHot xs) (indicesOneHot (length xs - 1) n)
>

This works!

> run2 :: [String] -> Maybe Int
> run2  =
>   (mapMaybe (readMaybe @(Int)))
>      >>> (subsequencesN 3)
>      >>> (find (sum >>> (==2020)))
>      >>> (fmap $ product)
