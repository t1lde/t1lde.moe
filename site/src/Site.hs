--------------------------------------------------------------------------------
import           Hakyll
--------------------------------------------------------------------------------
import Prelude hiding (fromList)
import qualified GHC.IO.Encoding as Enc
--------------------------------------------------------------------------------
import Resources
import Templates
import SiteConfig
import HakyllUtils
--------------------------------------------------------------------------------
main :: IO ()
main = do
  (Enc.setLocaleEncoding Enc.utf8)
  hakyllWith siteConfig siteRules
--------------------------------------------------------------------------------
siteRules :: Rules ()
siteRules = do
    sequenceA $ siteResources

    compileTemplates

    match "pages/index.markdown" $ do
      --route idRoute
      route $ constRoute "index.html"
      compileWithContext indexCtx (baseTemplate =<< indexPage)

    create [""] $ compile $ makeItem $ Redirect "index.html"

    match "pages/*.markdown" $ do
      route $ (setExtension "html") `composeRoutes` (setDirectory "")
      compileWithContext mempty (baseTemplate =<< (lift pandocCompiler))

    match "templates/posts.markdown" $ do
      route $ setExtension "html"
      compileWithContext mempty (baseTemplate =<< postsPage)





    --match (fromList ["pages/about.rst", "pages/contact.markdown"]) $ do
    --    route   $ setExtension "html"
    --    compile $ pandocCompiler
    --        >>= loadAndApplyTemplate "templates/default.html" baseCtx
    --        >>= relativizeUrls

    --match "posts/*" $ do
    --    route $ setExtension "html"
    --    compile $ pandocCompiler
    --        >>= loadAndApplyTemplate "templates/post.html"    postCtx
    --        >>= loadAndApplyTemplate "templates/default.html" postCtx
    --        >>= relativizeUrls

    --create ["pages/archive.html"] $ do
    --    route idRoute
    --    compile $ do
    --        posts <- recentFirst =<< loadAll "posts/*"
    --        let archiveCtx =
    --                listField "posts" postCtx (return posts) `mappend`
    --                constField "title" "Archives"            `mappend`
    --                baseCtx

    --        makeItem ""
    --            >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
    --            >>= loadAndApplyTemplate "templates/default.html" archiveCtx
    --            >>= relativizeUrls


    --match "pages/index.html" $ do
    --    route idRoute
    --    compile $ do
    --        posts <- recentFirst =<< loadAll "posts/*"
    --        let indexCtx =
    --                listField "posts" postCtx (return posts) `mappend`
    --                baseCtx

    --        getResourceBody
    --            >>= applyAsTemplate indexCtx
    --            >>= loadAndApplyTemplate "templates/default.html" indexCtx
    --            >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

compileWithContext ctx compiler = compile $ runReaderT compiler ctx
