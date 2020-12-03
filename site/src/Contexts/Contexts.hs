module Contexts.Contexts where
--------------------------------------------------------------------------------
import Data.Kind
import GHC.TypeLits
--------------------------------------------------------------------------------
import Contexts.HRecord
import Contexts.TypedContext
--------------------------------------------------------------------------------

class ContextField (name :: Symbol) (ty :: (FieldType group)) (context :: Type) (itemBody :: Type) where
  getField :: context -> (name := (FieldRepr ty itemBody) ::= ty)

instance {-# OVERLAPPING #-} ((FieldRepr ty itemBody) ~ val) => ContextField name ty (HList ((name := val ::= ty) ': tail) ) (itemBody :: Type) where
  getField :: HList ((name := val ::= ty) ': tail) -> (name := val ::= ty)
  getField (x ::: _) = x

instance {-# OVERLAPPABLE #-}( (ContextField name ty (HList tail) itemBody) ) => ContextField name ty (HList ((othername := otherval ::= otherty) ': tail)) itemBody where
  getField :: HList ((othername := otherval ::= otherty) ': tail) -> (name := (FieldRepr ty itemBody) ::= ty)
  getField (_ ::: xs) = getField @_ @name @(ty) @(HList tail) @(itemBody) xs


basicContext :: (ctx ~ (FieldsRepr '["name" :== 'StringField, "gender" :== ('OptionalField 'StringField), "homepage" :== 'UrlField ] String)) => HList ctx
basicContext
  =   (NamedVal @"name"     $ TypedVal @_ @_ @'StringField "Tilde")
  ::: (NamedVal @"gender"   $ TypedVal @_ @_ @('OptionalField 'StringField) Nothing)
  ::: (NamedVal @"homepage" $ TypedVal @_ @_ @('UrlField) (Url "www.t1lde.moe") )
  ::: HNil

nameField :: String
nameField = let (NamedVal (TypedVal name)) = getField @_ @"name" @'StringField basicContext in name
