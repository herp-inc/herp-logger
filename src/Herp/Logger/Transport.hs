{-# LANGUAGE PackageImports #-}

module Herp.Logger.Transport
    ( Transport(..)
    , TransportInput(..)
    ) where

import "text" Data.Text
import "aeson" Data.Aeson ( Object )
import "bytestring" Data.ByteString.Short (ShortByteString)
import Herp.Logger.LogLevel ( LogLevel )
import GHC.Generics (Generic)

data TransportInput = TransportInput
    { message :: Text
    , date :: ShortByteString
    , level :: LogLevel
    , extra :: Object
    }
    deriving stock Generic

data Transport = Transport
    { name         :: Text -- 種類を識別するための名前
    , runTransport :: TransportInput -> IO () -- スレッドセーフでなければならない
    , threshold    :: LogLevel
    , flush :: IO ()
    }
    deriving stock Generic
