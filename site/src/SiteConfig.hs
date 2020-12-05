module SiteConfig where
--------------------------------------------------------------------------------
import Hakyll
--------------------------------------------------------------------------------

siteConfig :: Configuration
siteConfig = defaultConfiguration { providerDirectory = "./site/"}

siteTitle :: String
siteTitle = "Tilde Website"

siteCssPath :: String
siteCssPath = "css"

siteFontsPath :: String
siteFontsPath = "fonts"

aboutPagePath :: String
aboutPagePath = "/aboutme.html"

postsPagePath :: String
postsPagePath = "/posts.html"

projectsPagePath :: String
projectsPagePath = "/projects.html"

contactPagePath :: String
contactPagePath = "/contact.html"
