module Templates where
--------------------------------------------------------------------------------
import Hakyll
import SiteConfig
import HakyllUtils
--------------------------------------------------------------------------------

compileTemplates :: Rules ()
compileTemplates = match ("templates/*") $ compile templateBodyCompiler

--------------------------------------------------------------------------------
-- Base page template, templates/base.html


siteUrls :: Context String
siteUrls = mconcat $ getZipList $
  constField
  <$> (ZipList [ "images_url",   "css_url",   "fonts_url",   "about_url",   "posts_url",   "projects_url",   "contact_url"])
  <*> (ZipList [ siteImagesPath, siteCssPath, siteFontsPath, aboutPagePath, postsPagePath, projectsPagePath, contactPagePath]
        <&> (toUrl))

baseCtx :: Context String
baseCtx = mconcat
  [ (metadataField <> titleField "title")
  , pathField "path"
  , bodyField "body"
  , constField "site_title" siteTitle
  , siteUrls
  , pinsField "pins" allPages
  ]

baseTemplate :: Item String -> ReaderT (Context String) Compiler (Item String)
baseTemplate item = ((lift .) relativizeUrls) =<< do
  footer <- fmap itemBody footerTemplate
  local (<> (baseCtx <> constField "footer" footer)) $ (liftCtx (loadAndApplyTemplate "templates/base.html") item)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
footerCtx :: Context String
footerCtx
  = constField "site_repo" siteRepo

footerTemplate :: ReaderT (Context String) Compiler (Item String)
footerTemplate = local (<> footerCtx) $
  (lift $ makeItem "")
    >>=  (liftCtx $ loadAndApplyTemplate "templates/footer.html")

--------------------------------------------------------------------------------
-- Index page
indexCtx :: Context String
indexCtx = mempty

indexPage :: ReaderT (Context String) Compiler (Item String)
indexPage = local (<> indexCtx) $
  (lift $ getResourceBody)
    >>= (liftCtx $ applyAsTemplate)
    >>= (lift <<< renderPandoc)

--------------------------------------------------------------------------------
-- Posts page
postsCtx :: Context String
postsCtx = listField "posts" (bodyField "post_preview") (runReaderT allPreviews mempty)

allPreviews :: ReaderT (Context String) Compiler [Item String]
allPreviews =
  (liftA2 (<>)
    (local (<>postCtx) $ postPreviews "posts/**.markdown")
    (local (<>literateHaskellCtx) $ postPreviews "posts/**.lhs"))
  >>= (lift <<< (mostRecentFirstMeta "published" metadataTimeParams))

postPreviews :: Pattern -> ReaderT (Context String) Compiler [Item String]
postPreviews pat =
  (lift $ loadAllSnapshots pat "preview")
    >>= (mapM (liftCtx $ loadAndApplyTemplate "templates/post_preview.html"))

postsPage :: ReaderT (Context String) Compiler (Item String)
postsPage = local (<> postsCtx) $
  (lift getResourceBody)
    >>= (liftCtx applyAsTemplate )
--------------------------------------------------------------------------------
-- Individual post page
postCtx :: Context String
postCtx = mconcat
  [ (urlField "post_url")
  , (titleMetaField "post_title" "title")
  , (dateMetaFieldISO8601 "post_date_utc" "published" metadataTimeParams)
  , (dateMetaFieldFromUTC outTimeFormat "post_date_local" "published" metadataTimeParams)
  , (teaserFieldWithSeparator previewSeparator "post_preview" "preview")
  , (bodyField "post_content")
  , metadataField
  ]

postPage :: ReaderT (Context String) Compiler (Item String)
postPage =
  local (<> postCtx) $
    (lift pandocCompiler)
      >>= (lift <<< saveSnapshot "preview")
      >>= (liftCtx $ loadAndApplyTemplate "templates/post.html")

--------------------------------------------------------------------------------

literateHaskellCtx :: Context String
literateHaskellCtx = mconcat
  [ (urlField "post_url")
  , (titleMetaField "post_title" "title")
  , (dateMetaFieldISO8601 "post_date_utc" "published" metadataTimeParams)
  , (dateMetaFieldFromUTC outTimeFormat "post_date_local" "published" metadataTimeParams)
  , (constField "post_preview" "...")--(teaserFieldWithSeparator previewSeparator "post_preview" "preview")
  , (bodyField "post_content")
  , metadataField
  ]

literateHaskellPage :: ReaderT (Context String) Compiler (Item String)
literateHaskellPage =
  local (<> literateHaskellCtx) $
    (lift pandocCompiler)
      >>= (lift <<< saveSnapshot "preview")
      >>= (liftCtx $ loadAndApplyTemplate "templates/literate_post.html")
      >>= (liftCtx $ loadAndApplyTemplate "templates/post.html")
--------------------------------------------------------------------------------
-- TODO: Use the tags system for this
aocCtx :: Context String
aocCtx = listField "posts" (bodyField "post_preview") ((runReaderT (postPreviews "posts/AOC/Day*.lhs") literateHaskellCtx) >>= (mostRecentFirstMeta "published" metadataTimeParams))

aocPage :: ReaderT (Context String) Compiler (Item String)
aocPage = local (<> aocCtx) $
  (lift $ getResourceBody)
    >>= (liftCtx $ applyAsTemplate)
