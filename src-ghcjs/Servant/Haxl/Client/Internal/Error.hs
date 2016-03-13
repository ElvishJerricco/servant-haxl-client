module Servant.Haxl.Client.Internal.Error
  ( ServantConnectionError(..)
  ) where

import           JavaScript.Web.XMLHttpRequest

data ServantConnectionError = ServantConnectionError XHRError deriving Show
