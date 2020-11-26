module Discogs.Types where

import Data.Aeson
import Deriving.Aeson

import Data.Kind
import GHC.TypeLits

type DiscogsJSONEncoding :: Symbol -> Type -> Type
type DiscogsJSONEncoding prefix =
  CustomJSON
  '[ TagSingleConstructors
   , FieldLabelModifier (StripPrefix prefix, CamelToSnake)
   , SumUntaggedValue
   ]

data DiscogsList = DiscogsList
  { listItems :: NonEmpty DiscogsListItem }
  deriving (Generic, Show)
  deriving (FromJSON) via DiscogsJSONEncoding "list" DiscogsList

data DiscogsListItem = DiscogsListItem
  { listItemComment :: Text
  , listItemDisplayTitle :: Text
  , listItemUri :: Text
  , listItemImageUrl :: Text
  }
  deriving (Generic, Show)
  deriving (FromJSON) via DiscogsJSONEncoding "listItem" DiscogsListItem
