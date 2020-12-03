module Resources
  ( siteResources
  ) where
--------------------------------------------------------------------------------
import Hakyll
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
fontResources =
  match "fonts_/*" $ do
    route idRoute
    compile copyFileCompiler

styleResources :: Rules ()
styleResources =
  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler
