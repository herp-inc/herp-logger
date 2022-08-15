{-# LANGUAGE CPP #-}

module Herp.Logger.StdoutTransport (
    stdoutTransport,
    stdoutANSITransport,
) where

import Herp.Logger.LogLevel
import Herp.Logger.Transport
import "fast-logger" System.Log.FastLogger (LoggerSet, ToLogStr (toLogStr), flushLogStr, pushLogStrLn)

import "aeson" Data.Aeson ((.=))
import "aeson" Data.Aeson qualified as A
import "aeson" Data.Aeson.Encoding qualified as A
import Data.ByteString.Short qualified as SB
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Herp.Logger.ANSI.Coloring (coloringLogInfoStr)

#if MIN_VERSION_aeson(2,0,0)
import "aeson" Data.Aeson.Key (fromText)
import "aeson" Data.Aeson.KeyMap as KM
#else
import Data.HashMap.Strict qualified as KM
import Data.Text (Text)
fromText :: Text -> Text
fromText = id
#endif

stdoutTransport' :: Text -> (LoggerSet -> TransportInput -> IO ()) -> LoggerSet -> LogLevel -> Transport
stdoutTransport' name push loggerSet transportThreshold =
    let runTransport input = push loggerSet input
        flush = flushLogStr loggerSet
    in Transport
        { name = name
        , threshold = transportThreshold
        , runTransport = runTransport
        , flush
        }

convertTransportInputToEncoding :: TransportInput -> A.Encoding
convertTransportInputToEncoding
    TransportInput{message, date, level, extra} =
        let series =
                ( "level" .= level
                    <> "date" .= T.decodeUtf8 (SB.fromShort date)
                    <> "message" .= message
                )
         in A.pairs $ series <> foldMap (uncurry (.=)) (KM.toList extra)

stdoutTransport :: LoggerSet -> LogLevel -> Transport
stdoutTransport = stdoutTransport' "stdout" push
  where
    push loggerSet input =
        let value = convertTransportInputToEncoding input
            json = A.encodingToLazyByteString value
         in pushLogStrLn loggerSet (toLogStr json)

stdoutANSITransport :: LoggerSet -> LogLevel -> Transport
stdoutANSITransport = stdoutTransport' "stdoutANSI" push
  where
    push loggerSet input@TransportInput{level} =
        let value = convertTransportInputToEncoding input
            json = A.encodingToLazyByteString value
         in pushLogStrLn loggerSet (toLogStr $ coloringLogInfoStr level json)
