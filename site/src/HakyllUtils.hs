module HakyllUtils where
--------------------------------------------------------------------------------
import Hakyll as Hakyll

import System.FilePath
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Format.ISO8601
import Data.Time.LocalTime

import Data.Binary
import Data.Bifunctor.Tannen
import Data.Bifunctor.Biff
import Data.Functor.Identity
--------------------------------------------------------------------------------
import Control.Arrow hiding (second, first)
--------------------------------------------------------------------------------
foldCompressCss :: (ToString a, Monoid a) => [Item a] -> Compiler (Item String)
foldCompressCss = makeItem . ((foldMap itemBody) >>> toString >>> compressCss)

setDirectory :: FilePath -> Routes
setDirectory dir = customRoute (toFilePath >>> (flip replaceDirectory dir))

hasFields :: [String] -> Metadata -> Bool
hasFields fields = (flip lookupString) >>> (flip traverse_ fields) >>> isJust

compileWithContext :: (Binary a, Typeable a, Writable a) => (Context a) -> ReaderT (Context a) Compiler (Item a) -> Rules ()
compileWithContext ctx compiler = compile $ runReaderT compiler ctx

compileCtx :: (Binary a, Typeable a, Writable a) => ReaderT (Context a) Compiler (Item a) -> Rules ()
compileCtx compiler = compile $ runReaderT compiler (mempty)

liftCtx :: (Context a -> Item a -> Compiler (Item b)) -> Item a -> ReaderT (Context a) Compiler (Item b)
liftCtx = flip >>> (>>> ReaderT)
--------------------------------------------------------------------------------
-- Build pinned items from metadata
data Pin = Pin {pinFirst :: Bool, pinItem :: Identifier, pinName :: String}

type Bf = Tannen Maybe (Biff ((,)) Identity ([]))
mkPins :: [(Identifier, String)] -> [Pin]
mkPins
  =   uncons
  >>> coerce >>> (bimap (uncurry $ Pin True) (uncurry $ Pin False)) >>> coerce @(Bf Pin Pin) @(Maybe (Pin, [Pin]))
  >>> (fmap $ uncurry (:)) >>> concat

buildPins :: Pattern -> Compiler [Item Pin]
buildPins pat  = do
  pins <- filterPins <$> (getAllMetadata pat)
  mapM makeItem $ mkPins pins
  where
    filterPins :: [(Identifier, Metadata)] -> [(Identifier, String)]
    filterPins
      = Tannen >>> (second $ lookupString "pin") >>> runTannen
      >>> (filter $ snd >>> isJust) >>> unzip >>> (second catMaybes) >>> (uncurry zip)

pinsField :: String -> Pattern -> Context a
pinsField field_name pat
  = (boolField "anypins" (const True)) <> (listField field_name pinCtx (buildPins pat))

pinCtx :: Context (Pin)
pinCtx
  =  (field "pin" (itemBody >>> pinName >>> return))
  <> (field "pin_url" (itemBody >>> pinItem >>> getRoute >>> (fmap $ (fromMaybe "") >>> toUrl ) ))
  <> (boolField "pin_first" (itemBody >>> pinFirst))
--------------------------------------------------------------------------------
-- Takes title from a named metadata field or falls back to basename
titleMetaField :: String -> String -> Context a
titleMetaField field_name meta_name =
  field field_name (metadataOrTitle meta_name)
  where
    metadataOrTitle :: String -> Item a -> Compiler String
    metadataOrTitle meta_name =
      itemIdentifier
        >>> ((toFilePath >>> takeBaseName >>> fromMaybe) &&& (flip (getMetadataField @(Compiler)) meta_name))
        >>> (uncurry fmap)
-- recentFirst but using metadataOrModification
mostRecentFirstMeta :: String -> TimeFormatParams -> [Item String] -> Compiler [Item String]
mostRecentFirstMeta meta_name fmt =
   (mapM $ (runKleisli $ (((dateMeta) &&& (sortMeta)) &&& (arr id))))
   >>> (fmap $ (sortOn $ fst >>> (Down *** id)) >>> (fmap $ snd) )
   where
     dateMeta :: Kleisli Compiler (Item a) UTCTime
     dateMeta = Kleisli $ metadataOrModification meta_name fmt

     sortMeta :: Kleisli Compiler (Item a) Int
     sortMeta =
       Kleisli
       (itemIdentifier
         >>> (flip (getMetadataField @Compiler) "sort")
         >>> (fmap $ ((>>= readMaybe @Int) >>> (fromMaybe 0))))

--------------------------------------------------------------------------------
data TimeFormatParams = TimeFormatParams
  { timeFormatStr :: String
  , timeLocale :: Maybe(TimeLocale)
  , timeZone   :: Maybe(TimeZone)
  }
  deriving stock (Show, Eq)

dateMetaFieldFromUTC :: (UTCTime -> String) -> String -> String -> TimeFormatParams -> Context String
dateMetaFieldFromUTC formatter field_name meta_name meta_fmt =
  field field_name ((metadataOrModification meta_name meta_fmt) >>> (fmap formatter))

dateMetaFieldISO8601 :: String -> String -> TimeFormatParams -> Context String
dateMetaFieldISO8601 = dateMetaFieldFromUTC iso8601Show

dateMetaFieldFormat :: String -> String -> TimeFormatParams -> TimeFormatParams -> Context String
dateMetaFieldFormat field_name meta_name meta_fmt out_fmt@(TimeFormatParams out_fmt_str out_locale out_tz)
  = dateMetaFieldFromUTC ((utcToLocalTime (fromMaybe utc out_tz)) >>> (formatTime (fromMaybe defaultTimeLocale out_locale) out_fmt_str)) field_name meta_name meta_fmt

metadataOrModification :: String -> TimeFormatParams -> Item a -> Compiler UTCTime
metadataOrModification meta_name fmt =
   itemIdentifier
     >>> (dateFromField meta_name fmt) &&& (getItemModificationTime)
     >>> (uncurry (<|>))

dateFromField :: String -> TimeFormatParams -> Identifier -> Compiler UTCTime
dateFromField meta_name (TimeFormatParams meta_fmt meta_locale (Just meta_tz)) =
  (flip (getMetadataField' @(Compiler)) meta_name)
    >>> (>>= (parseTimeM @(Compiler) @(LocalTime) True (fromMaybe defaultTimeLocale meta_locale) meta_fmt))
    >>> (fmap $ localTimeToUTC meta_tz)

dateFromField meta_name (TimeFormatParams meta_fmt meta_locale Nothing) =
  (flip (getMetadataField' @(Compiler)) meta_name)
    >>> (>>= (parseTimeM @(Compiler) @(ZonedTime) True (fromMaybe defaultTimeLocale meta_locale) meta_fmt))
    >>> (fmap $ zonedTimeToUTC)
--------------------------------------------------------------------------------

toSiteUrl :: FilePath -> FilePath -> String
toSiteUrl = (</>)
