-----------
title: 'Day 6: Custom Customs'
subtitle: 'Advent of Code 2020'
-----------

> module Day6 where
> import Data.List.Split
> import Data.Char
> import Data.List (nub, intersect)
> import Control.Arrow
> import Data.Foldable
> import Data.Maybe
> import Data.String as S
> import Relude.Extra.Foldable1

 <!--imports-->

> run1 :: String -> Int
> run1 =
>  (split . dropBlanks . dropDelims $ onSublist "\n\n")
>    >>> (fmap $ (filter $ not . isSpace) >>> nub >>> length)
>    >>> (foldr' (+) 0)
>
> run2 :: String -> Int
> run2 =
>  (split . dropBlanks . dropDelims $ onSublist "\n\n")
>    >>> (fmap S.lines)
>    >>> (mapMaybe $ (viaNonEmpty $ foldl1' intersect))
>    >>> (foldMap $ length >>> Sum)
>    >>> getSum
>
