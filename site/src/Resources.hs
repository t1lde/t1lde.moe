module Resources
  ( siteResources
  ) where
--------------------------------------------------------------------------------
import Hakyll
import System.FilePath
--------------------------------------------------------------------------------
import HakyllUtils
import SiteConfig
--------------------------------------------------------------------------------

siteResources :: [Rules ()]
siteResources =
  [ imageResources
  , fontResources
  , styleResources
  ]

imageResources :: Rules ()
imageResources =
  match "images/*" $ do
    route   (setDirectory siteImagesPath)
    compile copyFileCompiler

fontResources :: Rules ()
fontResources = do
  match (fromGlob "fonts_/*.woff" .||. fromGlob "fonts_/*.woff2") $ do
    route (setDirectory siteFontsPath)
    compile copyFileCompiler

  match (fromGlob "fonts_/woff2/*.woff2") $ do
    route (setDirectory (siteFontsPath </> "woff2"))
    compile copyFileCompiler

  match ("fonts_/*.css") $
    compile getResourceBody

  create ["fonts.css"] $ do
    route (setDirectory siteFontsPath)
    compile $
      (loadAll @String "fonts_/*.css")
         >>= foldCompressCss

styleResources :: Rules ()
styleResources =
  match "css/*" $ do
    route (setDirectory siteCssPath)
    compile compressCssCompiler
