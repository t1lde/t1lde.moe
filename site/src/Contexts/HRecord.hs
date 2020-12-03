{-# LANGUAGE UndecidableInstances #-}
module Contexts.HRecord where
--------------------------------------------------------------------------------
import GHC.TypeLits
import Data.Kind
--------------------------------------------------------------------------------
-- HRecord implemented as a HList with name-indexed values

-- Name-indexed values
infixl 6 :=
newtype (name :: Symbol) := (val :: Type) = NamedVal val

-- HList GADT
infixr 5 :::
data HList (xs :: [Type]) where
  HNil :: HList '[]
  (:::) :: x -> HList xs -> HList (x ': xs)

type family InvalidHRecordItem (x :: Type) where
  InvalidHRecordItem x = TypeError
    (     'Text "Invalid HRecord Item: "    ':<>: ('ShowType x)
    ':$$: 'Text "Expected type in the form (" ':<>: ('ShowType ("symbol" := Type)) ':<>: 'Text ")"
    )

-- HRecord type family to enforce all fields of a HList are name-indexed
type family HRecordItem (x :: Type) where
  HRecordItem (name := val) = (name := val)
  HRecordItem (otherwise)   = InvalidHRecordItem otherwise

type family HRecord (xs :: [Type]) where
  HRecord xs = HRecord' xs '[]

type family HRecord' (xs :: [Type]) (res :: [Type]) :: Type where
  HRecord' '[]                   res = HList res
  HRecord' ((name := val) ': xs) res = HRecord' xs ((name := val) ': res)
  HRecord' (otherwise     ': xs) res = InvalidHRecordItem otherwise

