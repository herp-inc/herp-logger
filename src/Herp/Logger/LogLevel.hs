{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Herp.Logger.LogLevel
    ( LogLevel(..)
    ) where

import "aeson"   Data.Aeson                     ( Value(String), ToJSON(..) )
import "base"    GHC.OverloadedLabels           ( IsLabel(fromLabel) )

-- | https://tools.ietf.org/html/rfc5424#section-6.2.1
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

instance ToJSON LogLevel where
    toJSON Emergency     = String "emerg"
    toJSON Alert         = String "alert"
    toJSON Critical      = String "crit"
    toJSON Error         = String "error"
    toJSON Warning       = String "warn"
    toJSON Notice        = String "notice"
    toJSON Informational = String "info"
    toJSON Debug         = String "debug"

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
