module App where

import Control.Monad.Except
import Control.Monad.Catch

import Control.Applicative()
import Data.Typeable

newtype AppIO a = AppIO { runAppIO :: (IO a) }
  deriving (Monad, MonadIO, Functor, Applicative, Alternative, MonadThrow, MonadCatch) via IO

class ErrMsg err where
  errMsg :: (IsString b) => err -> b

newtype ErrMsgShow err = ErrMsgShow err
instance (Show err) => ErrMsg (ErrMsgShow err) where
  errMsg (ErrMsgShow err) = show err

class (Typeable sub, Typeable super, Typeable (AnySub super), Show sub, ErrMsg sub) => sub :< super where
  toAnySub :: sub -> AnySub super
  fromAnySub :: AnySub super -> Maybe sub
  showErrMsg :: (IsString str) => (AnySub super) -> str

  toAnySub = AnySub
  fromAnySub = cast

  showErrMsg = errMsg

data AnySub super = forall sub. (sub :< super) => AnySub sub
  deriving (Typeable)

deriving instance Show (AnySub super)

deriving via (ErrMsgShow (AnySub super)) instance (ErrMsg (AnySub super))

instance (Typeable super, Typeable (AnySub super)) => Exception (AnySub super)

data AppErr

instance MonadError (AnySub AppErr) AppIO where
  throwError err = die $ errMsg err
  catchError = catch

type MonadError' err super m = (err :< super, MonadError (AnySub super) m)
