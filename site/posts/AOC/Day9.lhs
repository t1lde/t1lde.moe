-----------
title: 'Day 9: Encoding Error'
subtitle: 'Advent of Code 2020'
-----------

> module Day9 (run1, run2) where
> import Control.Monad
> import Data.Sequence as Seq
> import Data.Foldable (find, foldr', minimum, maximum)
> import Relude.Extra.Map

 <!--imports-->

Day 9
======

> windows :: Int ->  Seq Int -> Seq (Seq Int)
> windows n = Seq.tails >>> (fmap $ Seq.take n) >>> (Seq.filter (Seq.length >>> (==n)))

> sums :: Seq Int -> IntSet
> sums xs = foldMap one $ do
>   ix <- Seq.fromFunction (Seq.length xs) id
>   iy <- Seq.fromFunction ix id
>   maybe Seq.empty return $ liftA2 (+) (Seq.lookup ix xs) (Seq.lookup iy xs)

> invalidSum :: Seq Int -> Bool
> invalidSum (xs :|> x) = not $ x `member` (sums xs)

> run1 :: [Int] -> Maybe (Int)
> run1 =
>   Seq.fromList
>     >>> (windows 26)
>     >>> find (invalidSum)
>     >>> (>>= Seq.lookup 25)
>

> subranges :: Seq Int -> Seq (Seq Int)
> subranges xs = do
>   n  <- Seq.fromList [3..Seq.length xs]
>   lo <- Seq.fromList [0..(Seq.length xs) - n]
>   return $ Seq.take n $ Seq.drop lo xs

> run2 :: [Int] -> Maybe Int
> run2 xs = do
>   v <- run1 xs
>   xs
>     & (
>       Seq.fromList
>          >>> subranges
>          >>> (find $ (foldr' (+) 0) >>> (==v) )
>          >>> (fmap $ minimum &&& maximum)
>          >>> (fmap $ uncurry (+))
>       )
>
