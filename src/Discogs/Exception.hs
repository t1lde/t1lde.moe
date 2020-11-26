module Discogs.Exception where

import Control.Monad.Except()
import Network.HTTP.Types

import App

data DiscogsRequestException
  = DiscogsRequestStatusErr Status
  | DiscogsJSONDecodeErr String
  deriving Show
  deriving ErrMsg via (ErrMsgShow DiscogsRequestException)

instance DiscogsRequestException :< AppErr
