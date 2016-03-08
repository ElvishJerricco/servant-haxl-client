module Servant.Haxl.Client.Internal.Error
  ( ServantConnectionError(..)
  ) where

import           Network.HTTP.Client

data ServantConnectionError = ServantConnectionError HttpException deriving Show
