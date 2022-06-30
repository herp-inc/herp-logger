{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Herp.Logger.SentryTransport
    ( sentry
    ) where

import "text" Data.Text (Text, unpack)
import "aeson" Data.Aeson (Value)
import "unordered-containers" Data.HashMap.Strict (HashMap)
import "raven-haskell" System.Log.Raven (register)
import "raven-haskell" System.Log.Raven.Types
    ( SentryLevel(..)
    , SentryRecord(SentryRecord, srExtra)
    , SentryService
    )
import "unordered-containers" Data.HashMap.Strict qualified as HashMap
import Herp.Logger.LogLevel qualified as Level (LogLevel(..))
import Herp.Logger.Transport (Transport(..), TransportInput(..))


sentry :: SentryService -> Level.LogLevel -> Transport
sentry svc transportThreshold =
    let name = "sentry"
        flush = pure ()
        runTransport TransportInput { message, level, extra } = do
            let sentryLevel = convertLevel level
            register svc
                     "authz"
                     sentryLevel
                     (unpack message)
                     (\x@SentryRecord { srExtra } -> x { srExtra = srExtra <> mkSRExtra extra })
    in Transport
        { name = name
        , threshold = transportThreshold
        , runTransport = runTransport
        , flush
        }

mkSRExtra :: Maybe (Text, Value) -> HashMap String Value
mkSRExtra (Just (key, val)) = HashMap.singleton (unpack key) val
mkSRExtra Nothing = mempty

convertLevel :: Level.LogLevel -> SentryLevel
convertLevel Level.Emergency     = Fatal
convertLevel Level.Alert         = Fatal
convertLevel Level.Critical      = Fatal
convertLevel Level.Error         = Error
convertLevel Level.Warning       = Warning
convertLevel Level.Notice        = Info
convertLevel Level.Informational = Info
convertLevel Level.Debug         = Debug
