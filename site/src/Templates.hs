module Templates where
--------------------------------------------------------------------------------
import Hakyll
import SiteConfig
--------------------------------------------------------------------------------

compileTemplates :: Rules ()
compileTemplates = match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
-- Base page template, templates/base.html

baseCtx :: Context String
baseCtx = mconcat
  [ defaultContext
  , constField "site_title" siteTitle
  , constField "css_path" siteCssPath
  , constField "fonts_path" siteFontsPath
  , constField "about_url" aboutPagePath
  , constField "posts_url" postsPagePath
  , constField "projects_url" projectsPagePath
  , constField "contact_url" contactPagePath
  ]

baseTemplate :: Item String -> ReaderT (Context String) Compiler (Item String)
baseTemplate item = do
  ctx <- ask
  footer <- fmap itemBody footerTemplate
  lift $ loadAndApplyTemplate "templates/base.html"
    (mconcat [ctx, baseCtx, (constField "footer" footer)]) item

--------------------------------------------------------------------------------
footerCtx :: Context String
footerCtx
  = constField "site_repo" "a"

footerTemplate :: ReaderT (Context String) Compiler (Item String)
footerTemplate = do
  ctx <- ask
  lift $ (makeItem ""
    >>= loadAndApplyTemplate "templates/footer.html" (footerCtx `mappend` ctx))

--------------------------------------------------------------------------------
-- Index page
indexCtx :: Context String
indexCtx = mempty

indexPage :: ReaderT (Context String) Compiler (Item String)
indexPage = lift pandocCompiler
--------------------------------------------------------------------------------
-- Posts page
postsCtx :: Context String
postsCtx = mempty

postsPage :: ReaderT (Context String) Compiler (Item String)
postsPage = do
  ctx <- ask
  lift $
    getResourceBody
        >>= applyAsTemplate (ctx `mappend` postsCtx)
        >>= renderPandoc
