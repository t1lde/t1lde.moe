--------------------------------------------------------------------------------
import           Hakyll
--------------------------------------------------------------------------------
import Prelude hiding (fromList)
import qualified GHC.IO.Encoding as Enc
--------------------------------------------------------------------------------
import Resources

--------------------------------------------------------------------------------
main :: IO ()
main = do
  (Enc.setLocaleEncoding Enc.utf8)
  hakyllWith hakyllConfig siteRules
--------------------------------------------------------------------------------
siteRules :: Rules ()
siteRules = do
    sequenceA $ siteResources
    match (fromList ["pages/about.rst", "pages/contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" baseCtx
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["pages/archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    baseCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "pages/index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    baseCtx

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler
--------------------------------------------------------------------------------
siteTitle :: String
siteTitle = "Tilde Website"

hakyllConfig :: Configuration
hakyllConfig = defaultConfiguration { providerDirectory = "./site/"}
--------------------------------------------------------------------------------
baseCtx :: Context String
baseCtx
  =  defaultContext
  <> constField "site_title" siteTitle


postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    baseCtx
