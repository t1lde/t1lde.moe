module Contexts.TypedContext where
--------------------------------------------------------------------------------
import Data.Time.Clock
import Data.Time.Format
import Hakyll.Core.Item
import Hakyll.Core.Identifier
--------------------------------------------------------------------------------
import GHC.TypeLits hiding (Text)
import Data.Kind
--------------------------------------------------------------------------------
import Contexts.HRecord
--------------------------------------------------------------------------------
-- Field types used as a type tag in context records

-- Field groups used as GADT tag to bound recursion
data FieldTypeGroup = Complex | Base
  deriving (Show)

data FieldType (group :: FieldTypeGroup) where
  StringField         :: FieldType 'Base
  BoolField           :: FieldType 'Base
  IntField            :: FieldType 'Base
  FractionalField     :: FieldType 'Base
  DateField           :: FieldType 'Base
  FilePathField       :: FieldType 'Base
  UrlField            :: FieldType 'Base
  RenderedItemField   :: FieldType 'Base
  OptionalField       :: FieldType 'Base -> FieldType 'Base
  FromItemField       :: FieldType 'Base -> FieldType 'Base
  FromMetadataField   :: FieldType 'Base -> FieldType 'Base
  PartialField        :: [k] -> FieldType 'Base
  ListField           :: [k] -> FieldType 'Complex
  FunctionField       :: [FieldType 'Base] -> FieldType 'Base -> FieldType 'Complex

--------------------------------------------------------------------------------
-- Haskell types to represent hakyll context field types

-- Date fields, renderable given UTC, locale, and a format
data DateVal = DateVal
  { dateUTC :: UTCTime
  , locale  :: TimeLocale
  , fmt     :: (TimeLocale -> UTCTime -> Text)
  }

-- Partial templates, which possibly evaluate to a template with additional context vars
data PartialVal (fields :: [Type]) (itemBody :: Type) = PartialVal
  { partialTemplate    :: Identifier
  , partialContext     :: (TypedContext fields itemBody)
  }

-- Function fields, which have variadic unnamed args, and may use data in Item
newtype FnVal (params :: [FieldType 'Base]) (itemBody :: Type) (returns :: FieldType 'Base)
  = FnVal {runFnVal :: HList (FnParams params itemBody) -> (Item itemBody) -> (FieldRepr returns itemBody)}

-- Text type wrappers
newtype FPath = FPath Text
newtype Url = Url Text

--------------------------------------------------------------------------------
-- Helper type families

-- Haskell type repr of each field type
type family FieldRepr (ty :: k) (itemBody :: Type) :: Type where
  FieldRepr 'StringField            itemBody = String
  FieldRepr 'BoolField              itemBody = Bool
  FieldRepr 'IntField               itemBody = Integer
  FieldRepr 'FractionalField        itemBody = Double
  FieldRepr 'DateField              itemBody = DateVal
  FieldRepr 'FilePathField          itemBody = FPath
  FieldRepr 'UrlField               itemBody = Url
  FieldRepr 'RenderedItemField      itemBody = itemBody
  FieldRepr ('OptionalField ty)     itemBody = Maybe (FieldRepr ty itemBody)
  FieldRepr ('FromItemField ty)     itemBody = FieldRepr ty itemBody
  FieldRepr ('FromMetadataField ty) itemBody = FieldRepr ty itemBody
  FieldRepr ('PartialField fields)  itemBody = PartialVal fields itemBody
  FieldRepr ('ListField fields)     itemBody = [TypedContext fields itemBody]
  FieldRepr ('FunctionField ps r)   itemBody = FnVal ps itemBody r

-- Recurse over Fields type to construct repr of each value (with name and field-type index)
type family FieldsRepr (fields :: [*]) (itemBody :: Type) where
  FieldsRepr '[]                     itemBody = '[]
  FieldsRepr ((name :== field) ': xs) itemBody = (name := (FieldRepr field itemBody) ::= field) ': (FieldsRepr xs itemBody)

-- Construct the HList type param (FieldsRepr, without name indices) for FnParams,
type family FnParams (params :: [FieldType 'Base]) (itemBody :: Type) :: [Type] where
  FnParams '[]       itemBody = '[]
  FnParams (field ': xs) itemBody = ((FieldRepr field itemBody) ::= field)': (FnParams xs itemBody)


--------------------------------------------------------------------------------
-- TypedContext impl
-- FieldType indexed values
infixr 7 ::=
newtype (val :: Type) ::= (ty :: k) = TypedVal val

infixl 6 :==
data (name :: Symbol) :== (val :: (FieldType group)) = Field

--type Field = forall group name. name := (FieldType group)
--newtype Fields = Fields [forall group name. name := (FieldType group)]

-- Type family for correct construction of Fields

-- TypedContext
newtype TypedContext (fields :: [*]) (itemBody :: Type) = TypedContext (HRecord (FieldsRepr fields itemBody))
