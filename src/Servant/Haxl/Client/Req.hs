{-# LANGUAGE CPP                   #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module Servant.Haxl.Client.Req
  ( Req
  , ServantError
  , ServantResponse
  , ServantConnectionError
  , defReq
  , appendToPath
  , appendToMatrixParams
  , appendToQueryString
  , addHeader
  , setRQBody
  , displayHttpRequest
  , initServantClientState
  , performRequest
  , performRequestCT
  , performRequestNoBody
  ) where

import           Control.Monad
import           Control.Monad.Catch
import           Data.ByteString.Lazy               hiding (elem, filter, map,
                                                     null, pack)
import           Data.Proxy
import           Data.String.Conversions
import           Data.Text                          (Text)
import           Haxl.Core                          hiding (Request, catch)
import           Network.HTTP.Media
import           Network.HTTP.Types
import qualified Network.HTTP.Types.Header          as HTTP
import           Servant.API.ContentTypes
import           Servant.Common.Text
import           Servant.Haxl.Client.BaseUrl
import           Servant.Haxl.Client.Internal
import           Servant.Haxl.Client.Internal.Error
import           Servant.Haxl.Client.Types

defReq :: Req
defReq = Req "" [] Nothing [] []

appendToPath :: String -> Req -> Req
appendToPath p req =
  req { reqPath = reqPath req ++ "/" ++ p }

appendToMatrixParams :: String
                     -> Maybe String
                     -> Req
                     -> Req
appendToMatrixParams pname pvalue req =
  req { reqPath = reqPath req ++ ";" ++ pname ++ maybe "" ("=" ++) pvalue }

appendToQueryString :: Text       -- ^ param name
                    -> Maybe Text -- ^ param value
                    -> Req
                    -> Req
appendToQueryString pname pvalue req =
  req { qs = qs req ++ [(pname, pvalue)]
      }

addHeader :: ToText a => String -> a -> Req -> Req
addHeader name val req = req { headers = headers req
                                      ++ [(name, toText val)]
                             }

setRQBody :: ByteString -> MediaType -> Req -> Req
setRQBody b t req = req { reqBody = Just (b, t) }

-- * performing requests

displayHttpRequest :: Method -> String
displayHttpRequest httpmethod = "HTTP " ++ cs httpmethod ++ " request"

performRequest :: Method -> Req -> WantedStatusCodes -> BaseUrl -> GenHaxl () (Int, ByteString, MediaType, [HTTP.Header], ServantResponse)
performRequest m r w h = dataFetch $ ServantRequest m r w h

performRequestCT :: MimeUnrender ct result =>
  Proxy ct -> Method -> Req -> WantedStatusCodes -> BaseUrl -> GenHaxl () ([HTTP.Header], result)
performRequestCT ct reqMethod req wantedStatus reqHost = do
  let acceptCT = contentType ct
  (_status, respBody, respCT, hrds, _response) <-
    performRequest reqMethod (req { reqAccept = [acceptCT] }) wantedStatus reqHost
  unless (matches respCT acceptCT) $ throwM $ UnsupportedContentType respCT respBody
  case mimeUnrender ct respBody of
    Left err -> throwM $ DecodeFailure err respCT respBody
    Right val -> return (hrds, val)

performRequestNoBody :: Method -> Req -> WantedStatusCodes -> BaseUrl -> GenHaxl () ()
performRequestNoBody reqMethod req wantedStatus reqHost = do
  _ <- performRequest reqMethod req wantedStatus reqHost
  return ()
