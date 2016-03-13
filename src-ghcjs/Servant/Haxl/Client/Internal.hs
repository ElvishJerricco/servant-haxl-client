{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE TypeFamilies             #-}

module Servant.Haxl.Client.Internal
  ( ServantResponse(..)
  , ServantRequest(..)
  , initServantClientState
  ) where

import           Control.Concurrent.Async
import           Control.Concurrent.QSem
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import qualified Data.ByteString.Char8              as BS
import           Data.ByteString.Lazy               hiding (elem, filter, map,
                                                     null, pack)
import qualified Data.CaseInsensitive               as CI
import           Data.Hashable
import qualified Data.JSString                      as JSString
import           Data.JSString.Text
import           Data.Maybe
import           Data.String.Conversions
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import           Data.Text.Encoding
import           Haxl.Core                          hiding (Request, catch)
import qualified JavaScript.Web.XMLHttpRequest      as XHR
import           Network.HTTP.Media
import           Network.HTTP.Types
import qualified Network.HTTP.Types.Header          as HTTP
import           Network.URI
import           Servant.Haxl.Client.BaseUrl
import           Servant.Haxl.Client.Internal.Error
import           Servant.Haxl.Client.Types

-- foreign import javascript safe "new DataView($3,$1,$2)" js_dataView :: Int -> Int -> JSVal -> JSVal

data ServantResponse = ServantResponse (XHR.Response BS.ByteString)
instance Show ServantResponse where
  show (ServantResponse xhrResponse) = show $ XHR.contents xhrResponse

xhrMethodConversion :: Method -> XHR.Method
xhrMethodConversion method
  | method == methodGet    = XHR.GET
  | method == methodPost   = XHR.POST
  | method == methodPut    = XHR.PUT
  | method == methodDelete = XHR.DELETE
  | otherwise               = error "No such XHR method"

xhrRequestConversion :: XHR.Method -> Req -> BaseUrl -> XHR.Request
xhrRequestConversion method req (BaseUrl reqScheme reqHost reqPort) =
  setData XHR.Request
    { XHR.reqMethod           = method
    , XHR.reqURI              = JSString.pack . show $ nullURI
      { uriScheme = case reqScheme of
           Http  -> "http:"
           Https -> "https:"
      , uriAuthority = Just
          URIAuth { uriUserInfo = ""
                  , uriRegName = reqHost
                  , uriPort = ":" ++ show reqPort
                  }
      , uriPath = reqPath req
      , uriQuery = BS.unpack $ renderQuery True $ queryTextToQuery (qs req)
      }
    , XHR.reqLogin            = Nothing
    , XHR.reqHeaders          = (\(str, text) -> (JSString.pack str, textToJSString text))
                                <$> headers req
    , XHR.reqWithCredentials  = False
    , XHR.reqData             = XHR.NoData -- will be set by setData
    }
  where
    setData r = case reqBody req of
      Nothing          -> r
      Just (bs, media) -> do
        -- let (b, _, _) = fromByteString $ toStrict bs
        r { XHR.reqHeaders = XHR.reqHeaders r ++ [("Content-Type", JSString.pack . show $ media)]
          -- , XHR.reqData = XHR.TypedArrayData $ getUint8Array b
          -- Temporary workaround until I can solve this:
          -- http://stackoverflow.com/questions/35930517/sending-binary-data-with-ghcjs-xhr
          , XHR.reqData = XHR.StringData $ textToJSString $ decodeUtf8 $ toStrict bs
          }

performRequest_ :: Method -> Req -> WantedStatusCodes -> BaseUrl
               -> EitherT ServantError IO ( Int, ByteString, MediaType
                                          , [HTTP.Header], ServantResponse)
performRequest_ method req wantedStatus reqHost = do
  let xhrMethod = xhrMethodConversion method
  let xhrReq = xhrRequestConversion xhrMethod req reqHost
  eResponse <- liftIO $ catchXHRError $ XHR.xhrByteString xhrReq
  case eResponse of
    Left err ->
      left $ ConnectionError $ ServantConnectionError err
    Right response -> do
      let body = fromStrict $ fromMaybe "" (XHR.contents response)
          status_code = XHR.status response
      hrds <- liftIO $
          breakHeaders . textFromJSString <$> XHR.getAllResponseHeaders response
      ct <- case lookup "Content-Type" hrds of
                 Nothing -> pure $ "application"//"octet-stream"
                 Just t -> case parseAccept t of
                   Nothing -> left $ InvalidContentTypeHeader (cs t) body
                   Just t' -> pure t'
      unless (wantedStatus `wants` status_code) $
        left $ FailureResponse (Status status_code "") ct body
      return (status_code, body, ct, hrds, ServantResponse response)
      where
        wants AllCodes _ = True
        wants (SelectCodes codes) status_code = status_code `elem` codes

breakHeaders :: Text -> [HTTP.Header]
breakHeaders allHeaders = Text.splitOn "\r\n" allHeaders >>= breakHeader
  where
    breakHeader :: Text -> [HTTP.Header]
    breakHeader txHeader = header
      where
        (txName, txDataWithColon) = Text.breakOn ": " txHeader
        txData = Text.stripPrefix ": " txDataWithColon
        ciName = CI.mk $ encodeUtf8 txName
        bsData = encodeUtf8 <$> txData
        header = maybe [] (\d -> [(ciName, d)]) bsData

catchXHRError :: IO a -> IO (Either XHR.XHRError a)
catchXHRError action = catch (Right <$> action) (pure . Left)

data ServantRequest a where
  ServantRequest :: Method -> Req -> WantedStatusCodes -> BaseUrl ->
    ServantRequest (Int, ByteString, MediaType, [HTTP.Header], ServantResponse)

deriving instance Show (ServantRequest a)
deriving instance Eq (ServantRequest a)

instance Show1 ServantRequest where
  show1 = show

instance Hashable (ServantRequest a) where
  hashWithSalt s (ServantRequest m r w h) = hashWithSalt s (m, r, w, h)

instance DataSourceName ServantRequest where
  dataSourceName _ = "ServantRequest"

instance StateKey ServantRequest where
  data State ServantRequest = ServantRequestState Int

instance DataSource () ServantRequest where
  fetch (ServantRequestState numThreads) _ () requests =
    AsyncFetch $ \inner -> do
      sem <- newQSem numThreads
      asyncs <- mapM (handler sem) requests
      inner
      mapM_ wait asyncs
      where
        handler :: QSem -> BlockedFetch ServantRequest -> IO (Async ())
        handler sem (BlockedFetch (ServantRequest met req wantedStatus reqHost) rvar) =
          async $ bracket_ (waitQSem sem) (signalQSem sem) $ do
            e <- runEitherT $ performRequest_ met req wantedStatus reqHost
            case e of
              Left err -> putFailure rvar err
              Right a -> putSuccess rvar a
            return ()

initServantClientState :: Int -> IO (State ServantRequest)
initServantClientState = return . ServantRequestState
