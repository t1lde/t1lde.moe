--------------------------------------------------------------------------------
import           Hakyll
import System.FilePath
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

    match "pages/templated/index.markdown" $ do
      route $ (setDirectory siteRootPath) `composeRoutes` (setExtension "html")
      compileCtx (baseTemplate =<< indexPage)

    create [""] $ compile $ makeItem $ Redirect "index.html"

    match "pages/*.markdown" $ do
      route $ (setExtension "html") `composeRoutes` (setDirectory siteRootPath)
      compileCtx (baseTemplate =<< (lift pandocCompiler))

    match "pages/templated/posts.html" $ do
      route $ (setDirectory siteRootPath)
      compileCtx (baseTemplate =<< postsPage)

    match "pages/templated/AdventOfCode.html" $ do
      route $ (setDirectory (siteRootPath </> "AOC2020"))
      compileCtx (baseTemplate =<< aocPage)

    match allMarkdownPosts $ do
      route $ (setExtension "html") `composeRoutes` (setDirectory sitePostsPath)
      compileCtx (baseTemplate =<< postPage)

    match "deps_/AOC/Day*.lhs" $ do
      route $ (setExtension "html") `composeRoutes` (setDirectory (siteRootPath </> "AOC2020"))
      compileCtx (baseTemplate =<< literateHaskellPage)

    match "deps_/AOC/Day*.lhs" $ version "raw_lhs" $ do
      route $ (setDirectory (siteRootPath </> "AOC2020"))
      compile getResourceBody

    match (allLiteratePosts .&&. (complement "deps_/AOC/Day*.lhs")) $ do
      route $ (setExtension "html") `composeRoutes` (setDirectory (siteRootPath </> "haskell"))
      compileCtx (baseTemplate =<< literateHaskellPage)

    match (allLiteratePosts .&&. (complement "deps_/AOC/Day*.lhs")) $ version "raw_lhs" $ do
      route $ (setDirectory (siteRootPath </> "haskell"))
      compile getResourceBody





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

