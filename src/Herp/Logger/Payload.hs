{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
module Herp.Logger.Payload
  ( Payload(..)
  , level
  , message
  , object
  , messageString
  , messageShow
  , messageUtf8
  , messageUtf8Lazy
  , messageUtf8Builder
  , messageException
  ) where

import Control.Exception
import Data.Aeson (Object, KeyValue(..))
import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.Semigroup
import Data.String
import Data.Text (Text)
import Data.Text.Encoding
import Data.Typeable
import Generic.Data
import GHC.Exts
import GHC.OverloadedLabels
import Herp.Logger.LogLevel

data Payload = Payload
  { payloadLevel :: Max LogLevel
  , payloadMessage :: Text -- モノイドとして結合するためにあえて別フィールド
  , payloadObject :: Object
  }
  deriving stock Generic
  deriving (Semigroup, Monoid) via Generically Payload

instance IsList Payload where
  type Item Payload = Payload
  toList = pure
  fromList = mconcat

instance IsLabel k LogLevel => IsLabel k Payload where
  fromLabel = level (fromLabel @k)

level :: LogLevel -> Payload
level lvl = mempty { payloadLevel = Max lvl }

message :: Text -> Payload
message txt = mempty { payloadMessage = txt }

messageString :: String -> Payload
messageString = message . fromString

messageShow :: Show a => a -> Payload
messageShow = messageString . show

messageUtf8 :: ByteString -> Payload
messageUtf8 = message . decodeUtf8

messageUtf8Lazy :: BL.ByteString -> Payload
messageUtf8Lazy = messageUtf8 . BL.toStrict

messageUtf8Builder :: BB.Builder -> Payload
messageUtf8Builder = messageUtf8Lazy . BB.toLazyByteString

messageException :: Exception e => e -> Payload
messageException e = messageShow e
  <> "exception_type" .= show (typeOf e)

object :: Object -> Payload
object obj = mempty { payloadObject = obj }
{-# WARNING object "This might confuse Datadog!" #-}

instance IsString Payload where
  fromString = message . fromString

instance KeyValue Payload where
  key .= val = mempty { payloadObject = key .= val }