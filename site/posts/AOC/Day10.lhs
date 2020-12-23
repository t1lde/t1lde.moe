------------
title: 'Day 10: Adapter Array'
subtitle: 'Advent of Code 2020'
sort: 10
published: 'Wednesday 23 Dec 2020 05:31:10 GMT'
------------

> module Day10 (run1, run2) where
> --------------------------------------------------------------------------------
> import Data.List
> import Control.Arrow

 <!--imports-->

Day10
======


> run1 :: [Int] -> Int
> run1 =
>   sort
>     >>> (scanl (\(prev, v) a -> (a, a-prev)) (0,0))
>     >>> (fmap snd)
>     >>> (drop 1)
>     >>> ((filter (==1)) &&& (filter (==3)))
>     >>> (length *** (length >>> (+1)))
>     >>> (uncurry (*))

> run2 :: [Int] -> Int
> run2 = const 0
