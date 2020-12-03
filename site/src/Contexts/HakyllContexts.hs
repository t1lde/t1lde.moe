module Contexts.HakyllContexts where
--------------------------------------------------------------------------------
import Contexts.TypedContext
import Contexts.HRecord
--------------------------------------------------------------------------------

type DefaultContext = TypedContext
  '[ "body"  :== 'FromItemField 'RenderedItemField
   , "url"   :== 'FromItemField 'UrlField
   , "path"  :== 'FromItemField 'FilePathField
   , "title" :== 'FromItemField 'StringField
   ]

