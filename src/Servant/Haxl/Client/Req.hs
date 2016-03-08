{-# LANGUAGE CPP                   #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
module Servant.Haxl.Client.Req
  ( Req
  , ServantError
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

import           Control.Concurrent.Async
import           Control.Concurrent.QSem
import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch         (MonadThrow, throwM)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Data.ByteString.Lazy        hiding (elem, filter, map, null,
                                              pack)
import           Data.Hashable
import           Data.Proxy
import           Data.String
import           Data.String.Conversions
import           Data.Text                   (Text)
import           Data.Text.Encoding
import           Haxl.Core                   hiding (Request, catch)
import           Network.HTTP.Client         hiding (Proxy)
import           Network.HTTP.Client.TLS
import           Network.HTTP.Media
import           Network.HTTP.Types
import qualified Network.HTTP.Types.Header   as HTTP
import           Network.URI
import           Servant.API.ContentTypes
import           Servant.Common.Text
import           Servant.Haxl.Client.BaseUrl
import           Servant.Haxl.Client.Types

import qualified Network.HTTP.Client         as Client

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

reqToRequest :: MonadThrow m => Req -> BaseUrl -> m Request
reqToRequest req (BaseUrl reqScheme reqHost reqPort) =
    (setheaders . setAccept . setrqb . setQS) <$> parseUrl url

  where url = show $ nullURI { uriScheme = case reqScheme of
                                  Http  -> "http:"
                                  Https -> "https:"
                             , uriAuthority = Just
                                 URIAuth { uriUserInfo = ""
                                         , uriRegName = reqHost
                                         , uriPort = ":" ++ show reqPort
                                         }
                             , uriPath = reqPath req
                             }

        setrqb r = case reqBody req of
                     Nothing -> r
                     Just (b,t) -> r { requestBody = RequestBodyLBS b
                                     , requestHeaders = requestHeaders r
                                                     ++ [(hContentType, cs . show $ t)] }
        setQS = setQueryString $ queryTextToQuery (qs req)
        setheaders r = r { requestHeaders = requestHeaders r
                                         <> fmap toProperHeader (headers req) }
        setAccept r = r { requestHeaders = filter ((/= "Accept") . fst) (requestHeaders r)
                                        <> [("Accept", renderHeader $ reqAccept req)
                                              | not . null . reqAccept $ req] }
        toProperHeader (name, val) =
          (fromString name, encodeUtf8 val)


-- * performing requests

displayHttpRequest :: Method -> String
displayHttpRequest httpmethod = "HTTP " ++ cs httpmethod ++ " request"


performRequest_ :: Manager -> Method -> Req -> WantedStatusCodes -> BaseUrl
               -> EitherT ServantError IO ( Int, ByteString, MediaType
                                          , [HTTP.Header], Response ByteString)
performRequest_ manager reqMethod req wantedStatus reqHost = do
  partialRequest <- liftIO $ reqToRequest req reqHost

  let request = partialRequest { Client.method = reqMethod
                               , checkStatus = \ _status _headers _cookies -> Nothing
                               }

  eResponse <- liftIO $ catchHttpException $ Client.httpLbs request manager
  case eResponse of
    Left err ->
      left $ ConnectionError err

    Right response -> do
      let status = Client.responseStatus response
          body = Client.responseBody response
          hrds = Client.responseHeaders response
          status_code = statusCode status
      ct <- case lookup "Content-Type" $ Client.responseHeaders response of
                 Nothing -> pure $ "application"//"octet-stream"
                 Just t -> case parseAccept t of
                   Nothing -> left $ InvalidContentTypeHeader (cs t) body
                   Just t' -> pure t'
      unless (wantedStatus `wants` status_code) $
        left $ FailureResponse status ct body
      return (status_code, body, ct, hrds, response)
      where
        wants AllCodes _ = True
        wants (SelectCodes codes) status_code = status_code `elem` codes

catchHttpException :: IO a -> IO (Either HttpException a)
catchHttpException action =
  catch (Right <$> action) (pure . Left)

data ServantRequest a where
  ServantRequest :: Method -> Req -> WantedStatusCodes -> BaseUrl ->
    ServantRequest (Int, ByteString, MediaType, [HTTP.Header], Response ByteString)

deriving instance Show (ServantRequest a)
deriving instance Eq (ServantRequest a)

instance Show1 ServantRequest where
  show1 = show

instance Hashable (ServantRequest a) where
  hashWithSalt s (ServantRequest m r w h) = hashWithSalt s (m, r, w, h)

instance StateKey ServantRequest where
  data State ServantRequest = ServantRequestState Int Manager

instance DataSourceName ServantRequest where
  dataSourceName _ = "ServantRequest"

instance DataSource () ServantRequest where
  fetch (ServantRequestState numThreads manager) _ () requests = AsyncFetch $ \inner -> do
    sem <- newQSem numThreads
    asyncs <- mapM (handler sem) requests
    inner
    mapM_ wait asyncs
    where
      handler :: QSem -> BlockedFetch ServantRequest -> IO (Async ())
      handler sem (BlockedFetch ((ServantRequest met req wantedStatus reqHost) :: ServantRequest a) rvar) =
        async $ bracket_ (waitQSem sem) (signalQSem sem) $ do
          e <- runEitherT $ performRequest_ manager met req wantedStatus reqHost
          case e of
            Left err -> putFailure rvar err
            Right a -> putSuccess rvar a
          return ()

initServantClientState :: Int -> IO (State ServantRequest)
initServantClientState numThreads =
  ServantRequestState numThreads <$> newManager tlsManagerSettings

performRequest :: Method -> Req -> WantedStatusCodes -> BaseUrl -> GenHaxl () (Int, ByteString, MediaType, [HTTP.Header], Response ByteString)
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
