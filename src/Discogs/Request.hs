module Discogs.Request
  (fetchDiscogs) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Network.HTTP.Types
--import Network.HTTP.Types.Header

import Data.Aeson

import Discogs.Exception ()

import Data.Bifunctor ()
import Control.Arrow ()
import Control.Monad.Except

import App
import Discogs.Exception
import Discogs.Types

fetchDiscogs :: AppIO DiscogsList
fetchDiscogs = do
  man      <- liftIO $ newManager managerSettings
  (liftIO $ httpLbs req man)
    >>= handleResponse
    >>= (eitherDecode >>> (first (toAnySub . DiscogsJSONDecodeErr)) >>> liftEither)
  where
    managerSettings :: ManagerSettings
    managerSettings = tlsManagerSettings

    req :: Request
    req = (parseRequest_ "https://api.discogs.com/lists/615603")
      { method = "GET"
      , requestHeaders = appHeaders
      }

handleResponse :: (MonadError' DiscogsRequestException super m) => (Response a) -> m a
handleResponse response
  | (responseStatus response == ok200) = return $ responseBody response
  | otherwise                          = throwError $ toAnySub $ DiscogsRequestStatusErr (responseStatus response)

appHeaders :: [Header]
appHeaders =
  [ (hUserAgent, "MerzbowNumberVis/0.1.0.0")
  , (hAccept,    "application/vnd.discogs.v2.discogs+json")
  ]
