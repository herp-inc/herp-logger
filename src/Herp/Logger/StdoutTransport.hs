module Herp.Logger.StdoutTransport where

import Data.HashMap.Strict as HashMap
import "aeson" Data.Aeson ((.=))
import "aeson" Data.Aeson qualified as A
import "aeson" Data.Aeson.Encoding qualified as A
import "fast-logger" System.Log.FastLogger (LoggerSet, ToLogStr(toLogStr), pushLogStrLn, flushLogStr)
import Herp.Logger.Transport
import Herp.Logger.LogLevel
import Herp.Util.Lens hiding ((.=))

stdoutTransport :: LoggerSet -> LogLevel -> Transport
stdoutTransport loggerSet transportThreshold =
    let name = "stdout"
        runTransport TransportInput { message, date, level, extra } = do
            let series =
                    ( "level" .= level
                    <> "date" .= (date ^. from shortBS . utf8)
                    <> "message" .= message
                    )
            let value = A.pairs $ case extra of
                    Just (key, val) -> series <> key .= val
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
