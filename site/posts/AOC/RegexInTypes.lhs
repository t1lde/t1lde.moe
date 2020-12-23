-----------
title: 'Type Level Programming in Haskell: Regex in Types'
pin: 'Regex In Types'
published: 'Wednesday 23 Dec 2020 05:31:10 GMT'
-----------

> {-# LANGUAGE UndecidableInstances #-}
> module RegexInTypes where
> --------------------------------------------------------------------------------
> import Relude.Extra.Type
> import Relude.Extra.Foldable1
> --import Relude as Prelude
> --import Prelude hiding (head)
> import qualified Data.Text as Tx
> --------------------------------------------------------------------------------
> import Data.Kind
> import GHC.TypeLits hiding (Text)
> import GHC.Records
> import Control.Applicative
> import qualified Control.Arrow as Ar
> import Control.Arrow hiding (first, second, (+++))
> import Data.List.NonEmpty (some1)
> import qualified Data.List.NonEmpty as NE
> import Data.Ix (inRange)
> import Text.Show
> import Data.Char

> --------------------------------------------------------------------------------
> import Parser
> --------------------------------------------------------------------------------

 <!--imports-->

**TODO:** Actually document this somewhat... later.

I fully acknowledge how cursed this is.

***DO NOT UNDER ANY CIRCUMSTANCES*** try to compile this with optimisation on, it takes *forever*.


> newtype (name :: Symbol) := (ty :: Type) = Named {eraseName :: ty}
>   deriving (Semigroup, Monoid) via ty
>   deriving stock Functor
> instance (Show ty, KnownSymbol name) => Show (name := ty) where
>   show (Named x) = ((showString $ symbolVal (Proxy @(name))) . (showString " := ") . (shows x)) ""
>

> infixr 5 :::
> data HList (xs :: [Type]) where
>   HNil  :: HList '[]
>   (:::) :: head -> HList tail -> HList (head ': tail)

> instance Show (HList '[]) where
>   showsPrec 5 HNil = showString ""
>   showsPrec _ HNil = showString "HNil"
>
> instance (Show (HList tail), Show head) => Show (HList (head ': tail)) where
>   showsPrec 5 (x ::: xs) = (showString ", ") . (shows x) . (showsPrec 5 xs)
>   showsPrec _ (x ::: xs) = (showChar '[') . (shows x) . (showsPrec 5 xs) . (showChar ']')
>

> instance {-# Overlaps #-}
>   HasField name (HList ((name := a) ': xs)) a where
>   getField ((Named x) ::: _) = x

> instance (HasField name (HList xs) a) => HasField name (HList (x ': xs)) a where
>   getField (_ ::: xs) = getField @(name) @(HList xs) xs

> infixr 5 +++
> (+++) :: HList (xs) -> HList (ys) -> HList (xs ++ ys)
> (x ::: xs) +++ ys         = x ::: (xs +++ ys)
> HNil       +++ ys = ys
>


> data ParseExp
>   = S Symbol               -- Match whole strings (uncapturing group)
>   | Empty                  -- Match empty string (Do nothing)
>   | End
>   | P ParsePredicate       -- Single character predicates
>   | PMany ParsePredicate   -- Many character predicate
>   | ParseExp :|| ParseExp  -- Or rule |, try first rule, then second
>   | ParseExp :++ ParseExp  -- And Rule, match first rule and then second
>   | Many ParseExp          -- Zero or more occurrences *
>   | Some ParseExp          -- One or more occurrences ?
>   | Symbol :? ParseExp     -- Named capture group

> data ParsePredicate
>   = Symbol :- Symbol                 -- Character range expressions such as [0-9]
>   | Not ParsePredicate               -- Invert predicate
>   | Or ParsePredicate ParsePredicate -- Or/Union predicates
>   | WSpace

> class RunPredicate (pred :: ParsePredicate) where
>   runPredicate :: (Char -> Bool)
>
> instance (KnownSymbol a, KnownSymbol b) => RunPredicate (a :- b) where
>   runPredicate c =
>     (maybe True (<=c) $ viaNonEmpty head (symbolVal (Proxy @(a)))) &&
>     (maybe True (c<=) $ viaNonEmpty head (symbolVal (Proxy @(b))))
>
> instance (RunPredicate exp) => RunPredicate (Not exp) where
>   runPredicate = runPredicate @(exp) >>> not
>
> instance (RunPredicate a, RunPredicate b) => RunPredicate (Or a b) where
>   runPredicate = (runPredicate @(a)) ||^ (runPredicate @(b))
>
>
> instance RunPredicate WSpace where
>   runPredicate = isSpace

> type family Caps (names :: ParseExp) :: [Type] where
>   Caps (name ':? exp) = Flattened ((name := [Text]) ': (Caps exp))
>   Caps (a ':|| b)     = Flattened ((Caps a) ++ (Caps b))
>   Caps (a ':++ b)     = Flattened ((Caps a) ++ (Caps b))
>   Caps ('Many exp)    = Caps exp
>   Caps ('Some exp)    = Caps exp
>   Caps _              = '[]

> class Empties (ts :: [Type]) where
>   empties :: HList ts
>
> instance (Empties tail, Monoid head) => Empties ((name := head) ': tail) where
>   empties = (Named @(name) (mempty @(head))) ::: (empties @(tail))
>
> instance Empties '[] where
>   empties = HNil

> type family HereThere (x :: k) (xs :: [k]) :: [Bool] where
>   HereThere x '[]       = '[]
>   HereThere x (x ': xs) = 'True  ': HereThere x xs
>   HereThere x (y ': xs) = 'False ': HereThere x xs
>
> class SplitTs (t :: Type) (ts :: [Type]) (ixs :: [Bool]) where
>   type WithTs    t ts ixs :: [Type]
>   type WithoutTs t ts ixs :: [Type]
>   splitTs :: HList ts -> (HList (WithTs t ts ixs), HList (WithoutTs t ts ixs))

> instance SplitTs t '[] '[] where
>   type WithTs    t '[] '[] = '[]
>   type WithoutTs t '[] '[] = '[]
>   splitTs HNil = (HNil, HNil)

> instance (SplitTs t ts ixs) => SplitTs t (t ': ts) ('True ': ixs) where
>   type WithTs    t (t ': ts) ('True ': ixs) = t ': WithTs t ts ixs
>   type WithoutTs t (t ': ts) ('True ': ixs) = WithoutTs t ts ixs
>   splitTs (x ::: xs) = first (x :::) $ splitTs @(t) @(ts) @(ixs) xs
>
> instance (SplitTs t ts ixs) => SplitTs t (other ': ts) ('False ': ixs) where
>   type WithTs    t (other ': ts) ('False ': ixs) = WithTs t ts ixs
>   type WithoutTs t (other ': ts) ('False ': ixs) = other ': WithoutTs t ts ixs
>   splitTs (x ::: xs) = second (x :::) $ splitTs @(t) @(ts) @(ixs) xs
>

> type family Grouped (ts :: [Type]) :: [Type] where
>   Grouped '[]       = '[]
>   Grouped (x ': xs) = HList (WithTs x (x ': xs) (HereThere x (x ': xs))) ': Grouped (WithoutTs x xs (HereThere x xs))
>
> class GroupT (ts :: [Type]) where
>   groupT :: HList ts -> HList (Grouped ts)
>
> instance
>   ( (SplitTs t ts (HereThere t ts))
>   , (GroupT (WithoutTs t ts (HereThere t ts)))
>   )
>   => GroupT (t ': ts) where
>   groupT (x ::: xs) = uncurry (\withs withouts -> (x ::: withs) ::: groupT withouts) $ splitTs @(t) @(ts) @(HereThere t ts) xs
>
> instance GroupT '[] where
>   groupT = const HNil


> type family If (cond :: Bool) (a :: k) (b :: k) where
>   If 'True a _  = a
>   If 'False _ b = b
> type family Take (n :: Nat) (xs :: [k]) :: [k] where
>   Take 0 xs        = '[]
>   Take n '[]       = '[]
>   Take n (x ': xs) = x ': (Take (n-1) xs)
> type family Drop (n :: Nat) (xs :: [k]) :: [k] where
>   Drop 0 xs        = xs
>   Drop n '[]       = '[]
>   Drop n (x ': xs) = Drop (n-1) xs
> type family Length (xs :: [k]) :: Nat where
>   Length '[] = 0
>   Length (x ': xs) = (Length xs) + 1
>

>
> type Flattened xs = FlatFold (Grouped xs)
>

> type family AllEq (t :: Type) (ts :: [Type]) :: Constraint where
>   AllEq t '[]       = ()
>   AllEq t (x ': xs) = (t ~ x, AllEq t xs)

> hFoldMap :: (AllEq t ts, Monoid a) => (t -> a) -> HList ts -> a
> hFoldMap _ HNil = mempty
> hFoldMap f (x ::: xs) = (f x) <> (hFoldMap f xs)
>

> type family FlatFold (ts :: [Type]) :: [Type] where
>   FlatFold ((HList (x ': xs)) ': ys) = x ': FlatFold ys
>   FlatFold ((HList '[]) ': ys) = FlatFold ys
>   FlatFold '[] = '[]

> class Flatten (ts :: [Type]) where
>   flatfold :: HList ts -> HList (FlatFold ts)
>
> instance Flatten '[] where
>   flatfold HNil = HNil
>
> instance
>   (Flatten next, AllEq a ts, Monoid a)
>   => Flatten ((HList ts) ': next) where
>   flatfold (x@(_ ::: _) ::: xs) = (hFoldMap id x) ::: (flatfold @(next) xs)
>   flatfold (HNil ::: xs)        = flatfold @(next) xs
>
> flatten :: (Flatten (Grouped ts), GroupT ts) => HList ts -> HList (Flattened ts)
> flatten = groupT >>> flatfold
>
> merge ::
>   (Flatten (Grouped (as ++ bs)), GroupT (as ++ bs))
>   => HList as -> HList bs -> HList (Flattened (as ++ bs))
> merge xs ys = flatten (xs +++ ys)
>
> type Mergeable (xs :: [Type]) = ((Flatten (Grouped xs)), GroupT xs)


> class (Mergeable (Caps exp)) => RunExp (exp :: ParseExp) where
>   runExp :: Parser (Text, HList (Caps exp))
>   execExp :: Parser (HList (Caps exp))
>   execExp = snd <$> (runExp @exp)


> instance RunExp End where
>   runExp = matchEnd *> (pure ("", HNil))


> instance RunExp Empty where
>   runExp = pure ("", HNil)
>
>
> instance (KnownSymbol sym) => RunExp (S sym) where
>   runExp = fmap (,HNil) (matchText $ fromString $ symbolVal (Proxy @(sym)))
>
> instance (RunPredicate pred) => RunExp (P pred) where
>   runExp = fmap (, HNil) (fmap Tx.singleton $ charP $ runPredicate @(pred))

> instance
>   (RunPredicate pred)
>    => RunExp (PMany pred) where
>   runExp =
>     (whileP (runPredicate @(pred)))
>       <&> (,HNil)
>
> instance
>   ( RunExp exp
>   , Mergeable (Caps exp)
>   , Mergeable (Caps (sym ':? exp))
>   , Mergeable ('[sym := [a]])
>   , (Flattened ('[sym := [a]]) ++ (Caps exp)) ~ (Caps (sym ':? exp))
>   )
>    => RunExp  (sym ':? exp) where
>   runExp =  (uncurry $ \a b -> (a, merge ((Named @(sym) [a]) ::: HNil) b)) <$> (runExp @(exp))
>
> instance
>   ( RunExp exp
>   , Mergeable (Caps exp)
>   , Flattened (Caps exp ++ Caps exp) ~ (Caps exp)
>   , Mergeable (Caps exp ++ Caps exp)
>   , (Empties (Caps exp))
>   )
>   => RunExp (Many exp) where
>   runExp =
>     (many (runExp @(exp)))
>       <&> (unzip >>> (mconcat *** mergeCaptures))
>
>      where
>        mergeCaptures :: [HList (Caps exp)] -> HList (Caps exp)
>        mergeCaptures = ((viaNonEmpty (foldl1' merge)) >>> (fromMaybe (empties @(Caps exp))) )
>
>
>
> instance
>   ( RunExp exp
>   , Mergeable (Caps exp)
>   , (Flattened (Caps exp ++ Caps exp)) ~ (Caps exp)
>   , Mergeable (Caps exp ++ Caps exp)
>   , (Empties (Caps exp))
>   )
>   => RunExp (Some exp) where
>   runExp =
>     (some1 (runExp @(exp)))
>       <&> (NE.unzip >>> ((foldl1' (<>)) *** (foldl1' merge)))
>
>
> instance
>   (RunExp a, RunExp b, Mergeable (Caps (a :++ b)), Mergeable (Caps a), Mergeable (Caps b), Mergeable ((Caps a) ++ (Caps b)))
>   => RunExp (a :++ b) where
>   runExp = liftA2 (\(at, ac) (bt, bc) -> (at <> bt, ac `merge` bc))  (runExp @(a)) (runExp @(b))


> instance
>   ( RunExp a
>   , RunExp b
>   , Empties (Caps a)
>   , Empties (Caps b)
>   , Mergeable (Caps a ++ Caps b)
>   , Mergeable (Caps (a :|| b))
>   , Flatten (Grouped (Caps a ++ Caps b))
>   , GroupT (Caps a ++ Caps b)
>   )
>   => RunExp (a :|| b) where
>   runExp =
>     (fmap (Ar.second $ (`merge` (empties @(Caps b)))) (runExp @(a)))
>     <|>
>     (fmap (Ar.second $ ((empties @(Caps a)) `merge`)) (runExp @(b)))


> type family FnOf (args :: [Type]) (ret :: Type) :: Type where
>   FnOf '[]       ret = ret
>   FnOf ((name := x) ': xs) ret = x -> (FnOf xs ret)
>
> type family AllNames (args :: [Type]) (ty :: Type) :: Constraint where
>   AllNames '[] ty = ()
>   AllNames ((name := x) ': xs) ty = ((HasField name ty x), AllNames xs ty)
>
> class MapFields (fields :: [Type]) where
>   mapFields :: (AllNames fields (HList xs)) => HList (xs) -> (FnOf fields ret) -> ret
>
> instance (MapFields xs) => MapFields ((name := x) ': xs) where
>   mapFields xs f = (mapFields @(xs) xs) (f (getField @(name) xs))
>
> instance MapFields '[] where
>   mapFields xs f = f

> class GetFields (xs :: [Type]) (fs :: [Type]) where
>   getFields :: HList xs -> HList fs
>
> instance GetFields xs '[] where
>   getFields HNil = HNil
>
> instance (HasField name (HList ts) f, GetFields ts fs) => GetFields ts ((name := f) ': fs) where
>   getFields xs = ( (Named @(name) (getField @name xs)) ::: (getFields @(ts) @(fs) xs))
>
>
> class ApplyH (as :: [Type]) (fs :: [Type]) (bs :: [Type]) where
>   applyH :: HList (fs) -> HList (as) -> HList (bs)
> instance ApplyH '[] '[] '[] where
>   applyH HNil HNil = HNil
> instance (ApplyH as fs bs) => ApplyH (a ': as) ((a -> b) ': fs) (b ': bs) where
>   applyH (f ::: fs) (x ::: xs) = (f x) ::: (applyH @(as) @(fs) @(bs) fs xs)

> class TraverseH (as :: [Type]) (fs :: [Type]) (bs :: [Type]) (m :: Type -> Type) where
>   traverseH :: HList (fs) -> HList (as) -> m (HList (bs))
> instance (Applicative m) => TraverseH '[] '[] '[] m where
>   traverseH HNil HNil = pure HNil
> instance (TraverseH as fs bs m, Applicative m) => TraverseH (a ': as) ((a -> m b) ': fs) (b ': bs) m where
>   traverseH :: HList ((a -> m b) ': fs) -> HList (a ': as) -> m (HList (b ': bs))
>   traverseH (f ::: fs) (x ::: xs) = (:::) <$> (f x) <*> (traverseH @(as) @(fs) @(bs) fs xs)

> class HMap (as :: [Type]) (bs :: [Type]) a b where
>   hmap :: (a -> b) -> HList (as) -> HList (bs)
> instance HMap '[] '[] a b where
>   hmap f HNil = HNil
> instance (HMap as bs a b, Applicative m) => HMap (a ': as) (b ': bs) a b where
>   hmap f (x ::: xs) = (f x) ::: (hmap f xs)
> instance (HMap as bs a b, Applicative m) => HMap (other ': as) (other ': bs) a b where
>   hmap f (x ::: xs) = x ::: (hmap f xs)
> instance (HMap as bs a b, Applicative m) => HMap ((name := a) ': as) ((name := b) ': bs) a b where
>   hmap f ((Named x) ::: xs) = (Named @name (f x)) ::: (hmap f xs)
