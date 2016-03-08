module Servant.Haxl.Client.Internal.Error where

import           Network.HTTP.Client

data ServantConnectionError = ServantConnectionError HttpException deriving Show
