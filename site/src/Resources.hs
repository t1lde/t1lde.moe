module Resources
  ( siteResources
  ) where
--------------------------------------------------------------------------------
import Hakyll
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
    route   idRoute
    compile copyFileCompiler

fontResources :: Rules ()
fontResources = do
  match (fromGlob "fonts_/*.woff" .||. fromGlob "fonts_/*.woff2") $ do
    route (setDirectory siteFontsPath)
    compile copyFileCompiler

  match "fonts_/*.css" $
    compile getResourceBody

  create ["fonts/fonts.css"] $ do
    route idRoute
    compile $
      (loadAll @String "fonts_/*.css")
         >>= foldCompressCss

styleResources :: Rules ()
styleResources =
  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler
