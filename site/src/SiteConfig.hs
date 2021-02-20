module SiteConfig where
--------------------------------------------------------------------------------
import Hakyll
import System.FilePath

import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime

import Data.Text
--------------------------------------------------------------------------------
import Control.Arrow hiding (first)
--------------------------------------------------------------------------------
import HakyllUtils
--------------------------------------------------------------------------------

siteConfig :: Configuration
siteConfig
  = defaultConfiguration
  { providerDirectory = "./site/"
  , previewHost = "0.0.0.0"
  }


siteTitle :: String
siteTitle = "~"

siteRepo :: String
siteRepo = "https://gitlab.com/t1lde/t1lde.moe"
--------------------------------------------------------------------------------
siteRootPath :: FilePath
siteRootPath = ""

siteRootUrl :: String
siteRootUrl = "/"

siteCssPath :: FilePath
siteCssPath = siteRootPath </> "css"

siteFontsPath :: FilePath
siteFontsPath = siteRootPath </> "fonts"

siteImagesPath :: FilePath
siteImagesPath = siteRootPath </> "images"
--------------------------------------------------------------------------------
sitePostsPath :: FilePath
sitePostsPath = siteRootPath </> "posts"

siteDepsPath :: FilePath
siteDepsPath = siteRootPath </> "deps_"

aboutPagePath :: FilePath
aboutPagePath = siteRootPath </> "aboutme.html"

postsPagePath :: FilePath
postsPagePath = siteRootPath </> "posts.html"

projectsPagePath :: FilePath
projectsPagePath = siteRootPath </> "projects.html"

contactPagePath :: FilePath
contactPagePath = siteRootPath </> "contact.html"
--------------------------------------------------------------------------------
allPages :: Pattern
allPages
  =    (fromGlob "pages/**")
  .||. (fromGlob (sitePostsPath </> "**"))
  .||. allDepsPages

allDepsPages :: Pattern
allDepsPages
  =     (fromGlob (siteDepsPath </> "**.markdown"))
  .||.  (fromGlob (siteDepsPath </> "**.lhs"))

allMarkdownPosts :: Pattern
allMarkdownPosts
  =    (fromGlob (siteDepsPath </> "**.markdown"))
  .||. (fromGlob (sitePostsPath </> "**.markdown"))

allLiteratePosts :: Pattern
allLiteratePosts
  =    (fromGlob (siteDepsPath </> "**.lhs"))
  .||. (fromGlob (sitePostsPath </> "**.lhs"))
--------------------------------------------------------------------------------
previewSeparator :: String
previewSeparator = "<!--Preview-->"

metadataTimeParams :: TimeFormatParams
metadataTimeParams = (TimeFormatParams "%A %d %b %Y %T %Z" Nothing Nothing)

outTimeFormat :: UTCTime -> String
outTimeFormat =
  (utcToZonedTime $ TimeZone 0 False "GMT")
  >>> (formatTime defaultTimeLocale "%A the %-d") &&& ((formatTime defaultTimeLocale " of %B %Y %T %Z") >>> (flip (<>)))
  >>> (first $ thify)
  >>> (uncurry $ flip arr)
  where
    thify :: String -> String
    thify (toText -> txt)
      | "11" `isSuffixOf` txt = toString (txt <> "th")
      | "12" `isSuffixOf` txt = toString (txt <> "th")
      | "13" `isSuffixOf` txt = toString (txt <> "th")
      | "1"  `isSuffixOf` txt = toString (txt <> "st")
      | "2"  `isSuffixOf` txt = toString (txt <> "nd")
      | "3"  `isSuffixOf` txt = toString (txt <> "rd")
      | otherwise             = toString (txt <> "th")
