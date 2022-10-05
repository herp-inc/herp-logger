{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Herp.Logger.Transport.Sentry
    ( sentry
    ) where

import Data.Bifunctor (first)
import "text" Data.Text ( unpack)
import "aeson" Data.Aeson (Object, Value)
import "unordered-containers" Data.HashMap.Strict (HashMap)
import "raven-haskell" System.Log.Raven (register)
import "raven-haskell" System.Log.Raven.Types
    ( SentryLevel(..)
    , SentryRecord(SentryRecord, srExtra)
    , SentryService
    )
import "unordered-containers" Data.HashMap.Strict qualified as HashMap
import Herp.Logger.LogLevel qualified as Level (LogLevel(..))
import Herp.Logger.Transport.Types (Transport(..), TransportInput(..))


#if MIN_VERSION_aeson(2,0,0)
import "aeson" Data.Aeson.Key (toText)
import "aeson" Data.Aeson.KeyMap qualified as KM
#else
import Data.HashMap.Strict qualified as KM
import Data.Text (Text)
toText :: Text -> Text
toText = id
#endif

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

mkSRExtra :: Object -> HashMap String Value
mkSRExtra = HashMap.fromList . map (first (unpack . toText)) . KM.toList

convertLevel :: Level.LogLevel -> SentryLevel
convertLevel Level.Emergency     = Fatal
convertLevel Level.Alert         = Fatal
convertLevel Level.Critical      = Fatal
convertLevel Level.Error         = Error
convertLevel Level.Warning       = Warning
convertLevel Level.Notice        = Info
convertLevel Level.Informational = Info
convertLevel Level.Debug         = Debug
