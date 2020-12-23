-----------
title: 'Day 3: Toboggan Trajectory'
subtitle: 'Advent of Code 2020'
-----------

> module Day3 (run1, run2) where
> --------------------------------------------------------------------------------
> import qualified Data.Text as Tx
> import qualified Data.Set as S
> import Data.Bifunctor.Flip
> --------------------------------------------------------------------------------
> --import Relude as Prelude
> --import Prelude hiding (undefined, head)
> --------------------------------------------------------------------------------
> import Control.Arrow
> import qualified Control.Arrow as A
> import Data.Functor.Classes
> --------------------------------------------------------------------------------

 <!--imports-->

Day 3
====

Challenge 1
------

Representing the map as a Set of coordinates with trees

> newtype Coord = Coord (Int, Int)
>   deriving stock Show
>   deriving Eq via (Int,Int)

I need a newtype wrapper which does this for me :( .

> instance Ord Coord where
>   Coord (x1, y1) `compare` Coord (x2, y2) = (y1 `compare` y2) <> (x1 `compare` x2)

> xIndices :: Text -> [Int]
> xIndices = (Tx.breakOnAll "#") >>>  (fmap (fst >>> Tx.length))

> getBBox :: [Text] -> Coord
> getBBox = Coord <<< ((viaNonEmpty head) >>> (maybe 0 Tx.length)) &&& length

> xyIndices :: [Text] -> [Coord]
> xyIndices tx = (coerce @([(Int, Int)]) @([Coord])) $ join $ zipWith (\xs y -> fmap (,y) xs ) (fmap xIndices tx) [0..]

> mkTreesMap :: [Text] -> (Set Coord)
> mkTreesMap = (xyIndices >>> S.fromDistinctAscList)

> mkPath ::  Int -> Int -> Coord -> Set Coord
> mkPath right down (Coord (wrapx, boundy)) = S.fromDistinctAscList $ coerce $ zip (fmap (`mod` wrapx) [0, right..]) [0, down..boundy]
>


> run1 :: [Text] -> Int
> run1 =
>   ((getBBox >>> (mkPath 3 1)) &&& mkTreesMap)
>     >>> (uncurry S.intersection)
>     >>> (S.size)

> run2 :: [Text] -> Int
> run2 tx =
>   (fmap (S.intersection trees) paths)
>     & (foldMap (S.size >>> Product))
>     & getProduct
>    where
>       paths :: [Set Coord]
>       paths = fmap ($ bbox) $ zipWith mkPath [1,3,5,7,1] [1,1,1,1,2]
>
>       bbox :: Coord
>       bbox = getBBox tx
>
>       trees :: Set Coord
>       trees = mkTreesMap tx
>
>
>
>
