{-# LANGUAGE CPP #-}
module Herp.Logger.StdoutTransport where

import "fast-logger" System.Log.FastLogger (LoggerSet, ToLogStr(toLogStr), pushLogStrLn, flushLogStr)
import Herp.Logger.Transport
import Herp.Logger.LogLevel

import "aeson" Data.Aeson ((.=))
import "aeson" Data.Aeson qualified as A
import "aeson" Data.Aeson.Encoding qualified as A
import Data.ByteString.Short qualified as SB
import Data.Text.Encoding qualified as T

#if MIN_VERSION_aeson(2,0,0)
import "aeson" Data.Aeson.Key (fromText)
#else
import Data.Text
fromText :: Text -> Text
fromText = id
#endif

stdoutTransport :: LoggerSet -> LogLevel -> Transport
stdoutTransport loggerSet transportThreshold =
    let name = "stdout"
        runTransport TransportInput { message, date, level, extra } = do
            let series =
                    ( "level" .= level
                    <> "date" .= T.decodeUtf8 (SB.fromShort date)
                    <> "message" .= message
                    )
            let value = A.pairs $ case extra of
                    Just (key, val) -> series <> fromText key .= val
                    Nothing -> series
            let json = A.encodingToLazyByteString value
            pushLogStrLn loggerSet $ toLogStr json
        flush = flushLogStr loggerSet
    in Transport
        { name = name
        , threshold = transportThreshold
        , runTransport = runTransport
        , flush
        }
