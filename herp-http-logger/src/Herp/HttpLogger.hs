{-# LANGUAGE OverloadedLists #-}

module Herp.HttpLogger
  ( LogConfig (..)
  , logApplication_
  , logApplication
  ) where

import "base" GHC.Exts (fromList)
import "bytestring" Data.ByteString (ByteString)
import qualified "bytestring" Data.ByteString.Char8 as B8
import "case-insensitive" Data.CaseInsensitive (CI)
import qualified "case-insensitive" Data.CaseInsensitive as CI

import qualified "text" Data.Text.Lazy as LT
import "time" Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import "aeson" Data.Aeson (toJSON, Value (Object), Object)
import "wai" Network.Wai (Middleware, RequestBodyLength (..))
import qualified "wai" Network.Wai as Wai

import Protos.Herp.Log.HttpLog
  (HttpHeader (..), HttpQueryItem (..),
   HttpRequest (..), HttpResponse (..), HttpLog (..),
   HttpUserInfo (..))

data LogConfig =
  LogConfig
  { isFilteringOutPath :: ByteString -> Bool  -- ^ judge path-info to remove or not
  , removeHeaders  :: [ByteString]  -- ^ headers to remove
  }

logApplication :: (lvl -> msg -> Object -> IO ())
               -> (HttpLog -> lvl)
               -> (HttpLog -> msg)
               -> LogConfig
               -> (Wai.Request -> Wai.Response -> Maybe HttpUserInfo)
               -> Middleware {- Application -> Application -}
logApplication putLog lvl msg =
  logApplication_ (putHttpLog putLog lvl msg)

{-
-- 利用例:
_exampleApp :: Logger -> Middleware
_exampleApp logger waiApp =
   logApplication
     (logIO logger)
     (const Informational)
     (const "Wai HTTP log")
     (LogConfig (const False) ["cookie"])
     waiApp
 -}

putHttpLog :: (lvl -> msg -> Object -> IO ())
           -> (HttpLog -> lvl)
           -> (HttpLog -> msg)
           -> HttpLog -> IO ()
putHttpLog  putLog lvl msg log_ =
  case toJSON log_ of
    Object obj -> putLog (lvl log_) (msg log_) obj
    _          -> return ()

logApplication_ :: (HttpLog -> IO ())
                -> LogConfig
                -> (Wai.Request -> Wai.Response -> Maybe HttpUserInfo)
                -> Middleware {- Application -> Application -}
logApplication_ sendLog config getUserInfo app req respCallback0
  | isFilteringOutPath config $ Wai.rawPathInfo req  =
      app req respCallback0
  | otherwise  = do
      reqTs  <- getPOSIXTime
      let respCallback resp = do
            respTs <- getPOSIXTime
            sendLog $ mkLog (removeHeaders config) (reqTs, req) (respTs, resp) (getUserInfo req resp)
            respCallback0 resp
      app req respCallback

-----

mkLog :: [ByteString] -> (POSIXTime, Wai.Request) -> (POSIXTime, Wai.Response) -> Maybe HttpUserInfo -> HttpLog
mkLog rmHeaders0 (reqTs, req) (respTs, resp) userInfo =
    HttpLog
    { httpLogTimestampMs = toMillisecond reqTs
    , httpLogRequest = Just request_
    , httpLogResponse = Just response_
    , httpLogUserInfo = userInfo
    }
  where
    request_ =
      HttpRequest
      { httpRequestRequestMethod   =  bs8 $ Wai.requestMethod req
      , httpRequestHttpVersion     =  string $ show $ Wai.httpVersion req
      , httpRequestRawPathInfo     =  bs8 $ Wai.rawPathInfo req
      , httpRequestRequestHeaders  =  fromList $ filterHeaders rmHeaders0 $ Wai.requestHeaders req
      , httpRequestRemoteHost      =  string $ show $ Wai.remoteHost req
      , httpRequestQueryString     =  fromList [ HttpQueryItem  (bs8 a) (maybe mempty bs8 v) | (a, v) <- Wai.queryString req ]
                                     {- 本当に全てのクエリパラメータを出力してよいか. 長いものは出さないようにするべきか.
                                        よりフロント側でサイズ制限等で落とす. -}
      , httpRequestRequestBodyLength = runRBL (-1) fromIntegral $ Wai.requestBodyLength req
      }
    runRBL c _ ChunkedBody     = c
    runRBL _ k (KnownLength w) = k w
    response_ = HttpResponse (toMillisecond respTs) (string $ show $ Wai.responseStatus resp)
    toMillisecond = floor . (* 1000)

    bs8 = LT.pack . B8.unpack
    string = LT.pack

filterHeaders :: [ByteString] -> [(CI ByteString, ByteString)] -> [HttpHeader]
filterHeaders rmHeaders0 headers =
    [ HttpHeader (bs8 $ CI.original a) (bs8 v)
    | (a, v) <- headers
    , a `notElem` rmHeaders
    ]
  where
    rmHeaders = map CI.mk rmHeaders0 {- ++ defaultRemoveHeaders {- デフォルト値をハードコーディングするべきか? ["cookie"] とか -} -}
    bs8 = LT.pack . B8.unpack

_exampleFilterHeaders :: [HttpHeader]
_exampleFilterHeaders =
  filterHeaders ["cookie"]
  [ (CI.mk "cookie", "foo")
  , (CI.mk "Cookie", "bar")
  , (CI.mk "Content-Length", "1234")
  ]

-----

_exampleWaiLog :: HttpLog
_exampleWaiLog =
  HttpLog
  { httpLogTimestampMs = 0
  , httpLogRequest = Just req
  , httpLogResponse = Just resp
  , httpLogUserInfo = Just userInfo
  }
  where
    req =
      HttpRequest
      { httpRequestRequestMethod  = "GET"
      , httpRequestHttpVersion    = "1.1"
      , httpRequestRawPathInfo    = "/"
      , httpRequestRequestHeaders =
        [ HttpHeader "Host"              "example.com"
        , HttpHeader "User-Agent"        "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.114 Safari/537.36"
        , HttpHeader "Accept"            "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9"
        , HttpHeader "Accept-Encoding"   "gzip, deflate"
        , HttpHeader "Accept-Language"   "ja-JP,ja;q=0.9,en-US;q=0.8,en;q=0.7"
        ]
      , httpRequestRemoteHost     = "172.16.1.2:59550"
      , httpRequestQueryString    = []
      , httpRequestRequestBodyLength     = 0
      }
    resp =
      HttpResponse
      { httpResponseTimestampMs = 0
      , httpResponseResponseStatus = "200"
      }
    userInfo =
      HttpUserInfo
      { httpUserInfoUserId = "info@herp.co.jp"
      , httpUserInfoCompanyId = "C-464R6"
      }
