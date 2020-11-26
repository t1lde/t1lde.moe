module Lib
    ( discogsMain
    ) where

import Control.Arrow()

import Discogs.Request
import Discogs.Types
import App

discogsMain :: IO ()
discogsMain = do
  (runAppIO fetchDiscogs) >>= (listItems >>> head >>> show >>> putTextLn)
  return ()
