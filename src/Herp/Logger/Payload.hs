{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
module Herp.Logger.Payload
  ( Payload(..)
  , level
  , message
  , object
  ) where

import Data.Aeson (Object, KeyValue(..))
import Data.Semigroup
import Data.Monoid
import Data.String
import Data.Text (Text)
import Generic.Data
import GHC.OverloadedLabels
import Herp.Logger.LogLevel

data Payload = Payload
  { payloadLevel :: Max LogLevel
  , payloadMessage :: Text -- モノイドとして結合するためにあえて別フィールド
  , payloadObject :: Object
  }
  deriving stock Generic
  deriving (Semigroup, Monoid) via Generically Payload

instance IsLabel k LogLevel => IsLabel k Payload where
  fromLabel = level (fromLabel @k)

level :: LogLevel -> Payload
level lvl = mempty { payloadLevel = Max lvl }

message :: Text -> Payload
message txt = mempty { payloadMessage = txt }

object :: Object -> Payload
object obj = mempty { payloadObject = obj }

instance IsString Payload where
  fromString = message . fromString

instance KeyValue Payload where
  key .= val = mempty { payloadObject = key .= val }