module HakyllUtils where
--------------------------------------------------------------------------------
import Hakyll
import System.FilePath
--------------------------------------------------------------------------------
foldCompressCss :: (ToString a, Monoid a) => [Item a] -> Compiler (Item String)
foldCompressCss = makeItem . ((foldMap itemBody) >>> toString >>> compressCss)

setDirectory :: FilePath -> Routes
setDirectory dir = customRoute (toFilePath >>> (flip replaceDirectory dir))
