{-# LANGUAGE PackageImports #-}

module Herp.Logger.Transport
    ( Transport(..)
    , TransportInput(..)
    ) where

import "text" Data.Text
import "aeson" Data.Aeson ( Object, Value )
import "bytestring" Data.ByteString.Short (ShortByteString)
import Herp.Logger.LogLevel ( LogLevel )
import GHC.Generics (Generic)

data TransportInput = TransportInput
    { message :: Text
    , date :: ShortByteString -- See LoggingContext
    , level :: LogLevel
    , extra :: Maybe (Text, Value)
    }
    deriving stock Generic

data Transport = Transport
    { name         :: Text -- free word, for human
    , runTransport :: TransportInput -> IO () -- This task must be threadsafe
    , threshold    :: LogLevel
    , flush :: IO ()
    }
    deriving stock Generic
