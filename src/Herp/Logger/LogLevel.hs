{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Herp.Logger.LogLevel
    ( LogLevel(..)
    , parseLogLevel
    , convertLogLevel
    , convertLogLevelToStr
    ) where

import "aeson"   Data.Aeson                     ( Value(String), ToJSON(..), FromJSON(..) )
import "base"    GHC.OverloadedLabels           ( IsLabel(fromLabel) )
import Control.Monad.Logger qualified as M
import Data.Text (Text, toLower)
import Data.String (IsString)

-- | https://tools.ietf.org/html/rfc5424#section-6.2.1
-- https://scrapbox.io/herp-inc/RFC_draft_%E3%83%AD%E3%82%B0%E3%83%AC%E3%83%99%E3%83%AB%E3%81%AB%E3%81%AF_syslog_%E5%BD%A2%E5%BC%8F%E3%82%92%E4%BD%BF%E3%81%86
data LogLevel
    = Debug -- ^ debug-level messages
    | Informational -- ^ informational messages
    | Notice -- ^ normal but significant condition
    | Warning -- ^ warning conditions
    | Error -- ^ error conditions
    | Critical -- ^ critical conditions
    | Alert -- ^ action must be taken immediately
    | Emergency -- ^ system is unusable
    deriving stock Eq
    deriving stock Ord
    deriving stock Show
    deriving stock Bounded

-- | Convert 'M.LogLevel' to 'LogLevel'. Returns @'Left' str@ if there is no matching level.
convertLogLevel :: M.LogLevel -> Either Text LogLevel
convertLogLevel = \case
    M.LevelDebug -> Right Debug
    M.LevelInfo -> Right Informational
    M.LevelWarn -> Right Warning
    M.LevelError -> Right Error
    M.LevelOther (toLower -> "notice") -> Right Notice
    M.LevelOther (toLower -> "critical") -> Right Critical
    M.LevelOther (toLower -> "alert") -> Right Alert
    M.LevelOther (toLower -> "emergency") -> Right Emergency
    M.LevelOther level -> Left level

parseLogLevel :: String -> Either String LogLevel
parseLogLevel = \case
    "emerg" -> pure Emergency
    "alert" -> pure Alert
    "crit" -> pure Critical
    "error" -> pure Error
    "warn" -> pure Warning
    "notice" -> pure Notice
    "info" -> pure Informational
    "debug" -> pure Debug
    str -> Left $ "Unknown LogLevel: " <> show str

instance FromJSON LogLevel where
    parseJSON obj = parseJSON obj >>= either fail pure . parseLogLevel

convertLogLevelToStr :: (IsString a) => LogLevel -> a
convertLogLevelToStr = \case
    Emergency     -> "emerg"
    Alert         -> "alert"
    Critical      -> "crit"
    Error         -> "error"
    Warning       -> "warn"
    Notice        -> "notice"
    Informational -> "info"
    Debug         -> "debug"

instance ToJSON LogLevel where
    toJSON = String . convertLogLevelToStr

instance IsLabel "emerg" LogLevel where
    fromLabel = Emergency

instance IsLabel "alert" LogLevel where
    fromLabel = Alert

instance IsLabel "crit" LogLevel where
    fromLabel = Critical

instance IsLabel "error" LogLevel where
    fromLabel = Error

instance IsLabel "warn" LogLevel where
    fromLabel = Warning

instance IsLabel "notice" LogLevel where
    fromLabel = Notice

instance IsLabel "info" LogLevel where
    fromLabel = Informational

instance IsLabel "debug" LogLevel where
    fromLabel = Debug
