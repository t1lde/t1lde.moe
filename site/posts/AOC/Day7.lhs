-----------
title: 'Day 7: Handy Haversacks'
subtitle: 'Advent of Code 2020'
-----------

> {-# LANGUAGE QuasiQuotes #-}
> module Day7 where
> import Data.HashMap.Strict as HM hiding (foldl', filter, mapMaybe)
> import Control.Monad.State.Strict
> import Data.Char
> import Data.List (nub)
> import Text.Show
> import Prelude as R
> import RegexInTypes
> import GHC.Records
> import Parser

 <!--imports-->

Day7
======

The containment hierarchy (a partially ordered set?) of bags can be though of as a directed graph, with edges from each bag
to all the bags it can contain and labelling the edges with a capacity.

> type EdgeSet = HashMap Text Int

The 'inverse' graph, with each of the edges reversed, is useful to search outward from the
target "shiny gold" bag.

> data Graph = Graph {graph :: HashMap Text EdgeSet, inverse :: HashMap Text EdgeSet}

I tried to visualise with graphviz, but It got too messy.

> instance Show Graph where
>   show (Graph g inv) = toString $ mconcat
>     [ "digraph G{\n"
>     , foldlWithKey' (\tx a edges -> tx <> foldlWithKey' (\tx2 b label -> mconcat [tx2, (R.show @Text a), " -> ", (R.show @Text b), " [ label=\"", (R.show label), "\" ];\n"]) "" edges) "" g
>    -- , foldlWithKey' (\tx a edges -> tx <> foldlWithKey' (\tx2 b label -> mconcat [tx2, (R.show @Text a), " -> ", (R.show @Text b), " [ label=\"", (R.show $ negate label), "\" ];\n"]) "" edges) "" inv
>     , "}"
>     ]

Monoid instance overriding the default HashMap monoid to also merge the values.

> instance Semigroup Graph where
>   (Graph x  xinv) <> (Graph y yinv) = Graph (unionWith union x y) (unionWith union xinv yinv)
> instance Monoid Graph where
>   mempty = Graph mempty mempty

I tried to use regex-tdfa but was dissappointed by its functionality, a good (named) capture interface makes things
a lot nicer. I got to use my fancy regex-in-types that I overengineered on Day4.

> type BagsExp =
>   ("bag_name" ':? (
>     (PMany ("a" ':- "z")) ':++ (S " ") ':++ (PMany ("a" ':- "z"))
>   ))
>   ':++
>   (S " bags contain ")
>   ':++
>   ((Many (
>       (("contains_n") ':? (PMany ("0" ':- "9")))
>       ':++
>        (S " ")
>       ':++
>        ("contains_name" ':?
>          ((PMany ("a" ':- "z")) ':++ (S " ") ':++ (PMany ("a" ':- "z")))
>        )
>       ':++
>       (S " bag")
>       ':++
>       ((S "s") ':|| Empty)
>       ':++
>       ((S ", ") ':|| Empty)
>   ))
>   ':||
>   (S "no other bags")
>   )
>   ':++
>   (S ".")

Parsing the bags from the regex captures.
Constructing the graph as well as the inverse is pretty tricky.

> parseBags :: Text -> Graph
> parseBags t = fromMaybe mempty $ do
>   caps <- (execParser (execExp @BagsExp) t) :: Maybe (HList ["bag_name" := [Text], "contains_n" := [Text], "contains_name" := [Text]])
>   bag_name <- viaNonEmpty head (getField @"bag_name" caps)
>   contains_n <- traverse (toString >>> (readMaybe @Int)) $ getField @"contains_n" caps
>   let contains_name = (getField @"contains_name" caps)
>   let g = singleton bag_name $ HM.fromList $ zip contains_name contains_n
>   let inv =  HM.fromList $ zipWith (\name val -> (name, singleton bag_name val)) contains_name contains_n
>   return $ Graph g inv
>

Starting from a vertex, we have found a new path out from the original starting point,
then all other paths starting from here are appended.

> pathsFrom :: Text -> (HashMap Text EdgeSet) -> [[Text]]
> pathsFrom vertex g =
>     [vertex] : ((maybe [] keys (lookup vertex g))
>       >>= ((`pathsFrom` (delete vertex g)) >>> (fmap $ (vertex:))))


> run1 :: [Text] -> Int
> run1 =
>   foldMap parseBags
>     >>> inverse
>     >>> pathsFrom "shiny gold"
>     >>> (filter (length >>> (>1)))  -- Dont count the shiny gold bag itself
>     >>> mapMaybe (viaNonEmpty last) -- We have all combinations of bags over the shiny gold bag,
>     >>> nub                         -- but we actually want to know only the resulting outer bag
>     >>> length
>
>

> nBagsFrom :: Text -> (HashMap Text EdgeSet) -> Int
> nBagsFrom vertex g =
>   (lookupDefault mempty vertex g)
>     & (foldlWithKey' (\n k v -> n + v * (nBagsFrom k g)) 1)


> run2 :: [Text] -> Int
> run2 =
>   foldMap parseBags
>      >>> graph
>      >>> nBagsFrom "shiny gold"
>      >>> subtract 1
