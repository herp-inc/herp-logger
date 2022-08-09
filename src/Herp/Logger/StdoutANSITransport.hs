{-# LANGUAGE CPP #-}

module Herp.Logger.StdoutANSITransport where

import Herp.Logger.LogLevel
import Herp.Logger.Transport
import "fast-logger" System.Log.FastLogger (LoggerSet, ToLogStr (toLogStr), flushLogStr, pushLogStrLn)

import "aeson" Data.Aeson ((.=))
import "aeson" Data.Aeson qualified as A
import "aeson" Data.Aeson.Encoding qualified as A
import Data.ByteString.Short qualified as SB
import Data.Text.Encoding qualified as T

#if MIN_VERSION_aeson(2,0,0)
import "aeson" Data.Aeson.Key (fromText)
import "aeson" Data.Aeson.KeyMap as KM
import Herp.Logger.ANSI.Coloring (setLogColor, resetLogColor)
#else
import Data.HashMap.Strict qualified as KM
import Data.Text (Text)
fromText :: Text -> Text
fromText = id
#endif

stdoutANSITransport :: LoggerSet -> LogLevel -> Transport
stdoutANSITransport loggerSet transportThreshold =
    let name = "stdout"
        runTransport TransportInput{message, date, level, extra} = do
            let series =
                    ( "level" .= level
                        <> "date" .= T.decodeUtf8 (SB.fromShort date)
                        <> "message" .= message
                    )
            let value = A.pairs $ series <> foldMap (uncurry (.=)) (KM.toList extra)
            let json = A.encodingToLazyByteString value
            -- TODO: Fix to allow colorized output
            pushLogStrLn loggerSet $ toLogStr json
        flush = flushLogStr loggerSet
     in Transport
            { name = name
            , threshold = transportThreshold
            , runTransport = runTransport
            , flush
            }
