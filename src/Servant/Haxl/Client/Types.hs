{-# LANGUAGE DeriveGeneric #-}

module Servant.Haxl.Client.Types
  ( ServantError(..)
  , ServantConnectionError
  , Req(..)
  , WantedStatusCodes(..)
  , Scheme(..)
  , BaseUrl(..)
  ) where

import           Control.Exception
import           Data.ByteString.Lazy
import           Data.Hashable
import           Data.Text
import           GHC.Generics
import           Network.HTTP.Media
import           Network.HTTP.Types
import           Servant.Haxl.Client.Internal.Error

data ServantError
  = FailureResponse
    { responseStatus      :: Status
    , responseContentType :: MediaType
    , responseBody        :: ByteString
    }
  | DecodeFailure
    { decodeError         :: String
    , responseContentType :: MediaType
    , responseBody        :: ByteString
    }
  | UnsupportedContentType
    { responseContentType :: MediaType
    , responseBody        :: ByteString
    }
  | ConnectionError
    { connectionError :: ServantConnectionError
    }
  | InvalidContentTypeHeader
    { responseContentTypeHeader :: ByteString
    , responseBody              :: ByteString
    }
  deriving (Show)

instance Exception ServantError where

data Req = Req
  { reqPath   :: String
  , qs        :: QueryText
  , reqBody   :: Maybe (ByteString, MediaType)
  , reqAccept :: [MediaType]
  , headers   :: [(String, Text)]
  } deriving (Show, Eq, Ord, Generic)

instance Hashable Req where
  hashWithSalt s (Req p q b a h) = hashWithSalt s (p, q, bHash, aHash, h)
    where
      hashMediaType m = hashWithSalt s (show m)
      bHash = fmap (fmap hashMediaType) b
      aHash = fmap hashMediaType a

data WantedStatusCodes = AllCodes | SelectCodes [Int]
  deriving (Show, Eq, Ord, Generic)

instance Hashable WantedStatusCodes where
  hashWithSalt s AllCodes = hashWithSalt s (0::Int)
  hashWithSalt s (SelectCodes codes) = hashWithSalt s (1::Int, codes)

-- | URI scheme to use
data Scheme =
    Http  -- ^ http://
  | Https -- ^ https://
  deriving (Show, Eq, Ord, Generic)

instance Hashable Scheme where
  hashWithSalt s Http = hashWithSalt s (0 :: Int)
  hashWithSalt s Https = hashWithSalt s (1 :: Int)

-- | Simple data type to represent the target of HTTP requests
--   for servant's automatically-generated clients.
data BaseUrl = BaseUrl
  { baseUrlScheme :: Scheme -- ^ URI scheme to use
  , baseUrlHost   :: String   -- ^ host (eg "haskell.org")
  , baseUrlPort   :: Int      -- ^ port (eg 80)
  } deriving (Show, Eq, Ord, Generic)

instance Hashable BaseUrl where
  hashWithSalt s (BaseUrl sc h p) = hashWithSalt s (sc, h, p)
