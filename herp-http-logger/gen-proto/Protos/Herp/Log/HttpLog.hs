{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports       #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing       #-}
{-# OPTIONS_GHC -fno-warn-unused-matches       #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

-- | Generated by Haskell protocol buffer compiler. DO NOT EDIT!
module Protos.Herp.Log.HttpLog where
import qualified Prelude as Hs
import qualified Proto3.Suite.Class as HsProtobuf
import qualified Proto3.Suite.DotProto as HsProtobuf
import qualified Proto3.Suite.JSONPB as HsJSONPB
import Proto3.Suite.JSONPB ((.=), (.:))
import qualified Proto3.Suite.Types as HsProtobuf
import qualified Proto3.Wire as HsProtobuf
import qualified Control.Applicative as Hs
import Control.Applicative ((<*>), (<|>), (<$>))
import qualified Control.DeepSeq as Hs
import qualified Control.Monad as Hs
import qualified Data.ByteString as Hs
import qualified Data.Coerce as Hs
import qualified Data.Int as Hs (Int16, Int32, Int64)
import qualified Data.List.NonEmpty as Hs (NonEmpty(..))
import qualified Data.Map as Hs (Map, mapKeysMonotonic)
import qualified Data.Proxy as Proxy
import qualified Data.String as Hs (fromString)
import qualified Data.Text.Lazy as Hs (Text)
import qualified Data.Vector as Hs (Vector)
import qualified Data.Word as Hs (Word16, Word32, Word64)
import qualified GHC.Enum as Hs
import qualified GHC.Generics as Hs
import qualified Unsafe.Coerce as Hs

data HttpHeader = HttpHeader{httpHeaderAttribute :: Hs.Text,
                             httpHeaderValue :: Hs.Text}
                deriving (Hs.Show, Hs.Eq, Hs.Ord, Hs.Generic, Hs.NFData)

instance HsProtobuf.Named HttpHeader where
        nameOf _ = (Hs.fromString "HttpHeader")

instance HsProtobuf.HasDefault HttpHeader

instance HsProtobuf.Message HttpHeader where
        encodeMessage _
          HttpHeader{httpHeaderAttribute = httpHeaderAttribute,
                     httpHeaderValue = httpHeaderValue}
          = (Hs.mconcat
               [(HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 1)
                   httpHeaderAttribute),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 2)
                   httpHeaderValue)])
        decodeMessage _
          = (Hs.pure HttpHeader) <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 1))
              <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 2))
        dotProto _
          = [(HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 1)
                (HsProtobuf.Prim HsProtobuf.String)
                (HsProtobuf.Single "attribute")
                []
                ""),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 2)
                (HsProtobuf.Prim HsProtobuf.String)
                (HsProtobuf.Single "value")
                []
                "")]

instance HsJSONPB.ToJSONPB HttpHeader where
        toJSONPB (HttpHeader f1 f2)
          = (HsJSONPB.object ["attribute" .= f1, "value" .= f2])
        toEncodingPB (HttpHeader f1 f2)
          = (HsJSONPB.pairs ["attribute" .= f1, "value" .= f2])

instance HsJSONPB.FromJSONPB HttpHeader where
        parseJSONPB
          = (HsJSONPB.withObject "HttpHeader"
               (\ obj ->
                  (Hs.pure HttpHeader) <*> obj .: "attribute" <*> obj .: "value"))

instance HsJSONPB.ToJSON HttpHeader where
        toJSON = HsJSONPB.toAesonValue
        toEncoding = HsJSONPB.toAesonEncoding

instance HsJSONPB.FromJSON HttpHeader where
        parseJSON = HsJSONPB.parseJSONPB

instance HsJSONPB.ToSchema HttpHeader where
        declareNamedSchema _
          = do let declare_attribute = HsJSONPB.declareSchemaRef
               httpHeaderAttribute <- declare_attribute Proxy.Proxy
               let declare_value = HsJSONPB.declareSchemaRef
               httpHeaderValue <- declare_value Proxy.Proxy
               let _ = Hs.pure HttpHeader <*> HsJSONPB.asProxy declare_attribute
                         <*> HsJSONPB.asProxy declare_value
               Hs.return
                 (HsJSONPB.NamedSchema{HsJSONPB._namedSchemaName =
                                         Hs.Just "HttpHeader",
                                       HsJSONPB._namedSchemaSchema =
                                         Hs.mempty{HsJSONPB._schemaParamSchema =
                                                     Hs.mempty{HsJSONPB._paramSchemaType =
                                                                 Hs.Just HsJSONPB.SwaggerObject},
                                                   HsJSONPB._schemaProperties =
                                                     HsJSONPB.insOrdFromList
                                                       [("attribute", httpHeaderAttribute),
                                                        ("value", httpHeaderValue)]}})

data HttpQueryItem = HttpQueryItem{httpQueryItemAttribute ::
                                   Hs.Text,
                                   httpQueryItemValue :: Hs.Text}
                   deriving (Hs.Show, Hs.Eq, Hs.Ord, Hs.Generic, Hs.NFData)

instance HsProtobuf.Named HttpQueryItem where
        nameOf _ = (Hs.fromString "HttpQueryItem")

instance HsProtobuf.HasDefault HttpQueryItem

instance HsProtobuf.Message HttpQueryItem where
        encodeMessage _
          HttpQueryItem{httpQueryItemAttribute = httpQueryItemAttribute,
                        httpQueryItemValue = httpQueryItemValue}
          = (Hs.mconcat
               [(HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 1)
                   httpQueryItemAttribute),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 2)
                   httpQueryItemValue)])
        decodeMessage _
          = (Hs.pure HttpQueryItem) <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 1))
              <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 2))
        dotProto _
          = [(HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 1)
                (HsProtobuf.Prim HsProtobuf.String)
                (HsProtobuf.Single "attribute")
                []
                ""),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 2)
                (HsProtobuf.Prim HsProtobuf.String)
                (HsProtobuf.Single "value")
                []
                "")]

instance HsJSONPB.ToJSONPB HttpQueryItem where
        toJSONPB (HttpQueryItem f1 f2)
          = (HsJSONPB.object ["attribute" .= f1, "value" .= f2])
        toEncodingPB (HttpQueryItem f1 f2)
          = (HsJSONPB.pairs ["attribute" .= f1, "value" .= f2])

instance HsJSONPB.FromJSONPB HttpQueryItem where
        parseJSONPB
          = (HsJSONPB.withObject "HttpQueryItem"
               (\ obj ->
                  (Hs.pure HttpQueryItem) <*> obj .: "attribute" <*> obj .: "value"))

instance HsJSONPB.ToJSON HttpQueryItem where
        toJSON = HsJSONPB.toAesonValue
        toEncoding = HsJSONPB.toAesonEncoding

instance HsJSONPB.FromJSON HttpQueryItem where
        parseJSON = HsJSONPB.parseJSONPB

instance HsJSONPB.ToSchema HttpQueryItem where
        declareNamedSchema _
          = do let declare_attribute = HsJSONPB.declareSchemaRef
               httpQueryItemAttribute <- declare_attribute Proxy.Proxy
               let declare_value = HsJSONPB.declareSchemaRef
               httpQueryItemValue <- declare_value Proxy.Proxy
               let _ = Hs.pure HttpQueryItem <*>
                         HsJSONPB.asProxy declare_attribute
                         <*> HsJSONPB.asProxy declare_value
               Hs.return
                 (HsJSONPB.NamedSchema{HsJSONPB._namedSchemaName =
                                         Hs.Just "HttpQueryItem",
                                       HsJSONPB._namedSchemaSchema =
                                         Hs.mempty{HsJSONPB._schemaParamSchema =
                                                     Hs.mempty{HsJSONPB._paramSchemaType =
                                                                 Hs.Just HsJSONPB.SwaggerObject},
                                                   HsJSONPB._schemaProperties =
                                                     HsJSONPB.insOrdFromList
                                                       [("attribute", httpQueryItemAttribute),
                                                        ("value", httpQueryItemValue)]}})

data HttpRequest = HttpRequest{httpRequestRequestMethod :: Hs.Text,
                               httpRequestHttpVersion :: Hs.Text,
                               httpRequestRawPathInfo :: Hs.Text,
                               httpRequestRequestHeaders ::
                               Hs.Vector Protos.Herp.Log.HttpLog.HttpHeader,
                               httpRequestRemoteHost :: Hs.Text,
                               httpRequestQueryString ::
                               Hs.Vector Protos.Herp.Log.HttpLog.HttpQueryItem,
                               httpRequestRequestBodyLength :: Hs.Int64}
                 deriving (Hs.Show, Hs.Eq, Hs.Ord, Hs.Generic, Hs.NFData)

instance HsProtobuf.Named HttpRequest where
        nameOf _ = (Hs.fromString "HttpRequest")

instance HsProtobuf.HasDefault HttpRequest

instance HsProtobuf.Message HttpRequest where
        encodeMessage _
          HttpRequest{httpRequestRequestMethod = httpRequestRequestMethod,
                      httpRequestHttpVersion = httpRequestHttpVersion,
                      httpRequestRawPathInfo = httpRequestRawPathInfo,
                      httpRequestRequestHeaders = httpRequestRequestHeaders,
                      httpRequestRemoteHost = httpRequestRemoteHost,
                      httpRequestQueryString = httpRequestQueryString,
                      httpRequestRequestBodyLength = httpRequestRequestBodyLength}
          = (Hs.mconcat
               [(HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 1)
                   httpRequestRequestMethod),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 2)
                   httpRequestHttpVersion),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 3)
                   httpRequestRawPathInfo),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 5)
                   (Hs.coerce @(Hs.Vector Protos.Herp.Log.HttpLog.HttpHeader)
                      @(HsProtobuf.NestedVec Protos.Herp.Log.HttpLog.HttpHeader)
                      httpRequestRequestHeaders)),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 7)
                   httpRequestRemoteHost),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 9)
                   (Hs.coerce @(Hs.Vector Protos.Herp.Log.HttpLog.HttpQueryItem)
                      @(HsProtobuf.NestedVec Protos.Herp.Log.HttpLog.HttpQueryItem)
                      httpRequestQueryString)),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 12)
                   httpRequestRequestBodyLength)])
        decodeMessage _
          = (Hs.pure HttpRequest) <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 1))
              <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 2))
              <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 3))
              <*>
              (Hs.coerce
                 @(_ (HsProtobuf.NestedVec Protos.Herp.Log.HttpLog.HttpHeader))
                 @(_ (Hs.Vector Protos.Herp.Log.HttpLog.HttpHeader))
                 (HsProtobuf.at HsProtobuf.decodeMessageField
                    (HsProtobuf.FieldNumber 5)))
              <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 7))
              <*>
              (Hs.coerce
                 @(_ (HsProtobuf.NestedVec Protos.Herp.Log.HttpLog.HttpQueryItem))
                 @(_ (Hs.Vector Protos.Herp.Log.HttpLog.HttpQueryItem))
                 (HsProtobuf.at HsProtobuf.decodeMessageField
                    (HsProtobuf.FieldNumber 9)))
              <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 12))
        dotProto _
          = [(HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 1)
                (HsProtobuf.Prim HsProtobuf.String)
                (HsProtobuf.Single "request_method")
                []
                ""),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 2)
                (HsProtobuf.Prim HsProtobuf.String)
                (HsProtobuf.Single "http_version")
                []
                ""),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 3)
                (HsProtobuf.Prim HsProtobuf.String)
                (HsProtobuf.Single "raw_path_info")
                []
                ""),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 5)
                (HsProtobuf.Repeated
                   (HsProtobuf.Named (HsProtobuf.Single "HttpHeader")))
                (HsProtobuf.Single "request_headers")
                []
                ""),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 7)
                (HsProtobuf.Prim HsProtobuf.String)
                (HsProtobuf.Single "remote_host")
                []
                ""),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 9)
                (HsProtobuf.Repeated
                   (HsProtobuf.Named (HsProtobuf.Single "HttpQueryItem")))
                (HsProtobuf.Single "query_string")
                []
                ""),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 12)
                (HsProtobuf.Prim HsProtobuf.Int64)
                (HsProtobuf.Single "request_body_length")
                []
                "")]

instance HsJSONPB.ToJSONPB HttpRequest where
        toJSONPB (HttpRequest f1 f2 f3 f5 f7 f9 f12)
          = (HsJSONPB.object
               ["request_method" .= f1, "http_version" .= f2,
                "raw_path_info" .= f3, "request_headers" .= f5,
                "remote_host" .= f7, "query_string" .= f9,
                "request_body_length" .= f12])
        toEncodingPB (HttpRequest f1 f2 f3 f5 f7 f9 f12)
          = (HsJSONPB.pairs
               ["request_method" .= f1, "http_version" .= f2,
                "raw_path_info" .= f3, "request_headers" .= f5,
                "remote_host" .= f7, "query_string" .= f9,
                "request_body_length" .= f12])

instance HsJSONPB.FromJSONPB HttpRequest where
        parseJSONPB
          = (HsJSONPB.withObject "HttpRequest"
               (\ obj ->
                  (Hs.pure HttpRequest) <*> obj .: "request_method" <*>
                    obj .: "http_version"
                    <*> obj .: "raw_path_info"
                    <*> obj .: "request_headers"
                    <*> obj .: "remote_host"
                    <*> obj .: "query_string"
                    <*> obj .: "request_body_length"))

instance HsJSONPB.ToJSON HttpRequest where
        toJSON = HsJSONPB.toAesonValue
        toEncoding = HsJSONPB.toAesonEncoding

instance HsJSONPB.FromJSON HttpRequest where
        parseJSON = HsJSONPB.parseJSONPB

instance HsJSONPB.ToSchema HttpRequest where
        declareNamedSchema _
          = do let declare_request_method = HsJSONPB.declareSchemaRef
               httpRequestRequestMethod <- declare_request_method Proxy.Proxy
               let declare_http_version = HsJSONPB.declareSchemaRef
               httpRequestHttpVersion <- declare_http_version Proxy.Proxy
               let declare_raw_path_info = HsJSONPB.declareSchemaRef
               httpRequestRawPathInfo <- declare_raw_path_info Proxy.Proxy
               let declare_request_headers = HsJSONPB.declareSchemaRef
               httpRequestRequestHeaders <- declare_request_headers Proxy.Proxy
               let declare_remote_host = HsJSONPB.declareSchemaRef
               httpRequestRemoteHost <- declare_remote_host Proxy.Proxy
               let declare_query_string = HsJSONPB.declareSchemaRef
               httpRequestQueryString <- declare_query_string Proxy.Proxy
               let declare_request_body_length = HsJSONPB.declareSchemaRef
               httpRequestRequestBodyLength <- declare_request_body_length
                                                 Proxy.Proxy
               let _ = Hs.pure HttpRequest <*>
                         HsJSONPB.asProxy declare_request_method
                         <*> HsJSONPB.asProxy declare_http_version
                         <*> HsJSONPB.asProxy declare_raw_path_info
                         <*> HsJSONPB.asProxy declare_request_headers
                         <*> HsJSONPB.asProxy declare_remote_host
                         <*> HsJSONPB.asProxy declare_query_string
                         <*> HsJSONPB.asProxy declare_request_body_length
               Hs.return
                 (HsJSONPB.NamedSchema{HsJSONPB._namedSchemaName =
                                         Hs.Just "HttpRequest",
                                       HsJSONPB._namedSchemaSchema =
                                         Hs.mempty{HsJSONPB._schemaParamSchema =
                                                     Hs.mempty{HsJSONPB._paramSchemaType =
                                                                 Hs.Just HsJSONPB.SwaggerObject},
                                                   HsJSONPB._schemaProperties =
                                                     HsJSONPB.insOrdFromList
                                                       [("request_method",
                                                         httpRequestRequestMethod),
                                                        ("http_version", httpRequestHttpVersion),
                                                        ("raw_path_info", httpRequestRawPathInfo),
                                                        ("request_headers",
                                                         httpRequestRequestHeaders),
                                                        ("remote_host", httpRequestRemoteHost),
                                                        ("query_string", httpRequestQueryString),
                                                        ("request_body_length",
                                                         httpRequestRequestBodyLength)]}})

data HttpResponse = HttpResponse{httpResponseTimestampMs ::
                                 Hs.Int64,
                                 httpResponseResponseStatus :: Hs.Text}
                  deriving (Hs.Show, Hs.Eq, Hs.Ord, Hs.Generic, Hs.NFData)

instance HsProtobuf.Named HttpResponse where
        nameOf _ = (Hs.fromString "HttpResponse")

instance HsProtobuf.HasDefault HttpResponse

instance HsProtobuf.Message HttpResponse where
        encodeMessage _
          HttpResponse{httpResponseTimestampMs = httpResponseTimestampMs,
                       httpResponseResponseStatus = httpResponseResponseStatus}
          = (Hs.mconcat
               [(HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 1)
                   httpResponseTimestampMs),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 2)
                   httpResponseResponseStatus)])
        decodeMessage _
          = (Hs.pure HttpResponse) <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 1))
              <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 2))
        dotProto _
          = [(HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 1)
                (HsProtobuf.Prim HsProtobuf.Int64)
                (HsProtobuf.Single "timestamp_ms")
                []
                ""),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 2)
                (HsProtobuf.Prim HsProtobuf.String)
                (HsProtobuf.Single "response_status")
                []
                "")]

instance HsJSONPB.ToJSONPB HttpResponse where
        toJSONPB (HttpResponse f1 f2)
          = (HsJSONPB.object ["timestamp_ms" .= f1, "response_status" .= f2])
        toEncodingPB (HttpResponse f1 f2)
          = (HsJSONPB.pairs ["timestamp_ms" .= f1, "response_status" .= f2])

instance HsJSONPB.FromJSONPB HttpResponse where
        parseJSONPB
          = (HsJSONPB.withObject "HttpResponse"
               (\ obj ->
                  (Hs.pure HttpResponse) <*> obj .: "timestamp_ms" <*>
                    obj .: "response_status"))

instance HsJSONPB.ToJSON HttpResponse where
        toJSON = HsJSONPB.toAesonValue
        toEncoding = HsJSONPB.toAesonEncoding

instance HsJSONPB.FromJSON HttpResponse where
        parseJSON = HsJSONPB.parseJSONPB

instance HsJSONPB.ToSchema HttpResponse where
        declareNamedSchema _
          = do let declare_timestamp_ms = HsJSONPB.declareSchemaRef
               httpResponseTimestampMs <- declare_timestamp_ms Proxy.Proxy
               let declare_response_status = HsJSONPB.declareSchemaRef
               httpResponseResponseStatus <- declare_response_status Proxy.Proxy
               let _ = Hs.pure HttpResponse <*>
                         HsJSONPB.asProxy declare_timestamp_ms
                         <*> HsJSONPB.asProxy declare_response_status
               Hs.return
                 (HsJSONPB.NamedSchema{HsJSONPB._namedSchemaName =
                                         Hs.Just "HttpResponse",
                                       HsJSONPB._namedSchemaSchema =
                                         Hs.mempty{HsJSONPB._schemaParamSchema =
                                                     Hs.mempty{HsJSONPB._paramSchemaType =
                                                                 Hs.Just HsJSONPB.SwaggerObject},
                                                   HsJSONPB._schemaProperties =
                                                     HsJSONPB.insOrdFromList
                                                       [("timestamp_ms", httpResponseTimestampMs),
                                                        ("response_status",
                                                         httpResponseResponseStatus)]}})

data HttpUserInfo = HttpUserInfo{httpUserInfoUserId :: Hs.Text,
                                 httpUserInfoCompanyId :: Hs.Text}
                  deriving (Hs.Show, Hs.Eq, Hs.Ord, Hs.Generic, Hs.NFData)

instance HsProtobuf.Named HttpUserInfo where
        nameOf _ = (Hs.fromString "HttpUserInfo")

instance HsProtobuf.HasDefault HttpUserInfo

instance HsProtobuf.Message HttpUserInfo where
        encodeMessage _
          HttpUserInfo{httpUserInfoUserId = httpUserInfoUserId,
                       httpUserInfoCompanyId = httpUserInfoCompanyId}
          = (Hs.mconcat
               [(HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 1)
                   httpUserInfoUserId),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 2)
                   httpUserInfoCompanyId)])
        decodeMessage _
          = (Hs.pure HttpUserInfo) <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 1))
              <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 2))
        dotProto _
          = [(HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 1)
                (HsProtobuf.Prim HsProtobuf.String)
                (HsProtobuf.Single "user_id")
                []
                ""),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 2)
                (HsProtobuf.Prim HsProtobuf.String)
                (HsProtobuf.Single "company_id")
                []
                "")]

instance HsJSONPB.ToJSONPB HttpUserInfo where
        toJSONPB (HttpUserInfo f1 f2)
          = (HsJSONPB.object ["user_id" .= f1, "company_id" .= f2])
        toEncodingPB (HttpUserInfo f1 f2)
          = (HsJSONPB.pairs ["user_id" .= f1, "company_id" .= f2])

instance HsJSONPB.FromJSONPB HttpUserInfo where
        parseJSONPB
          = (HsJSONPB.withObject "HttpUserInfo"
               (\ obj ->
                  (Hs.pure HttpUserInfo) <*> obj .: "user_id" <*>
                    obj .: "company_id"))

instance HsJSONPB.ToJSON HttpUserInfo where
        toJSON = HsJSONPB.toAesonValue
        toEncoding = HsJSONPB.toAesonEncoding

instance HsJSONPB.FromJSON HttpUserInfo where
        parseJSON = HsJSONPB.parseJSONPB

instance HsJSONPB.ToSchema HttpUserInfo where
        declareNamedSchema _
          = do let declare_user_id = HsJSONPB.declareSchemaRef
               httpUserInfoUserId <- declare_user_id Proxy.Proxy
               let declare_company_id = HsJSONPB.declareSchemaRef
               httpUserInfoCompanyId <- declare_company_id Proxy.Proxy
               let _ = Hs.pure HttpUserInfo <*> HsJSONPB.asProxy declare_user_id
                         <*> HsJSONPB.asProxy declare_company_id
               Hs.return
                 (HsJSONPB.NamedSchema{HsJSONPB._namedSchemaName =
                                         Hs.Just "HttpUserInfo",
                                       HsJSONPB._namedSchemaSchema =
                                         Hs.mempty{HsJSONPB._schemaParamSchema =
                                                     Hs.mempty{HsJSONPB._paramSchemaType =
                                                                 Hs.Just HsJSONPB.SwaggerObject},
                                                   HsJSONPB._schemaProperties =
                                                     HsJSONPB.insOrdFromList
                                                       [("user_id", httpUserInfoUserId),
                                                        ("company_id", httpUserInfoCompanyId)]}})

data HttpLog = HttpLog{httpLogTimestampMs :: Hs.Int64,
                       httpLogRequest :: Hs.Maybe Protos.Herp.Log.HttpLog.HttpRequest,
                       httpLogResponse :: Hs.Maybe Protos.Herp.Log.HttpLog.HttpResponse,
                       httpLogUserInfo :: Hs.Maybe Protos.Herp.Log.HttpLog.HttpUserInfo}
             deriving (Hs.Show, Hs.Eq, Hs.Ord, Hs.Generic, Hs.NFData)

instance HsProtobuf.Named HttpLog where
        nameOf _ = (Hs.fromString "HttpLog")

instance HsProtobuf.HasDefault HttpLog

instance HsProtobuf.Message HttpLog where
        encodeMessage _
          HttpLog{httpLogTimestampMs = httpLogTimestampMs,
                  httpLogRequest = httpLogRequest, httpLogResponse = httpLogResponse,
                  httpLogUserInfo = httpLogUserInfo}
          = (Hs.mconcat
               [(HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 1)
                   httpLogTimestampMs),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 10)
                   (Hs.coerce @(Hs.Maybe Protos.Herp.Log.HttpLog.HttpRequest)
                      @(HsProtobuf.Nested Protos.Herp.Log.HttpLog.HttpRequest)
                      httpLogRequest)),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 11)
                   (Hs.coerce @(Hs.Maybe Protos.Herp.Log.HttpLog.HttpResponse)
                      @(HsProtobuf.Nested Protos.Herp.Log.HttpLog.HttpResponse)
                      httpLogResponse)),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 12)
                   (Hs.coerce @(Hs.Maybe Protos.Herp.Log.HttpLog.HttpUserInfo)
                      @(HsProtobuf.Nested Protos.Herp.Log.HttpLog.HttpUserInfo)
                      httpLogUserInfo))])
        decodeMessage _
          = (Hs.pure HttpLog) <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 1))
              <*>
              (Hs.coerce
                 @(_ (HsProtobuf.Nested Protos.Herp.Log.HttpLog.HttpRequest))
                 @(_ (Hs.Maybe Protos.Herp.Log.HttpLog.HttpRequest))
                 (HsProtobuf.at HsProtobuf.decodeMessageField
                    (HsProtobuf.FieldNumber 10)))
              <*>
              (Hs.coerce
                 @(_ (HsProtobuf.Nested Protos.Herp.Log.HttpLog.HttpResponse))
                 @(_ (Hs.Maybe Protos.Herp.Log.HttpLog.HttpResponse))
                 (HsProtobuf.at HsProtobuf.decodeMessageField
                    (HsProtobuf.FieldNumber 11)))
              <*>
              (Hs.coerce
                 @(_ (HsProtobuf.Nested Protos.Herp.Log.HttpLog.HttpUserInfo))
                 @(_ (Hs.Maybe Protos.Herp.Log.HttpLog.HttpUserInfo))
                 (HsProtobuf.at HsProtobuf.decodeMessageField
                    (HsProtobuf.FieldNumber 12)))
        dotProto _
          = [(HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 1)
                (HsProtobuf.Prim HsProtobuf.Int64)
                (HsProtobuf.Single "timestamp_ms")
                []
                ""),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 10)
                (HsProtobuf.Prim
                   (HsProtobuf.Named (HsProtobuf.Single "HttpRequest")))
                (HsProtobuf.Single "request")
                []
                ""),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 11)
                (HsProtobuf.Prim
                   (HsProtobuf.Named (HsProtobuf.Single "HttpResponse")))
                (HsProtobuf.Single "response")
                []
                ""),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 12)
                (HsProtobuf.Prim
                   (HsProtobuf.Named (HsProtobuf.Single "HttpUserInfo")))
                (HsProtobuf.Single "user_info")
                []
                "")]

instance HsJSONPB.ToJSONPB HttpLog where
        toJSONPB (HttpLog f1 f10 f11 f12)
          = (HsJSONPB.object
               ["timestamp_ms" .= f1, "request" .= f10, "response" .= f11,
                "user_info" .= f12])
        toEncodingPB (HttpLog f1 f10 f11 f12)
          = (HsJSONPB.pairs
               ["timestamp_ms" .= f1, "request" .= f10, "response" .= f11,
                "user_info" .= f12])

instance HsJSONPB.FromJSONPB HttpLog where
        parseJSONPB
          = (HsJSONPB.withObject "HttpLog"
               (\ obj ->
                  (Hs.pure HttpLog) <*> obj .: "timestamp_ms" <*> obj .: "request"
                    <*> obj .: "response"
                    <*> obj .: "user_info"))

instance HsJSONPB.ToJSON HttpLog where
        toJSON = HsJSONPB.toAesonValue
        toEncoding = HsJSONPB.toAesonEncoding

instance HsJSONPB.FromJSON HttpLog where
        parseJSON = HsJSONPB.parseJSONPB

instance HsJSONPB.ToSchema HttpLog where
        declareNamedSchema _
          = do let declare_timestamp_ms = HsJSONPB.declareSchemaRef
               httpLogTimestampMs <- declare_timestamp_ms Proxy.Proxy
               let declare_request = HsJSONPB.declareSchemaRef
               httpLogRequest <- declare_request Proxy.Proxy
               let declare_response = HsJSONPB.declareSchemaRef
               httpLogResponse <- declare_response Proxy.Proxy
               let declare_user_info = HsJSONPB.declareSchemaRef
               httpLogUserInfo <- declare_user_info Proxy.Proxy
               let _ = Hs.pure HttpLog <*> HsJSONPB.asProxy declare_timestamp_ms
                         <*> HsJSONPB.asProxy declare_request
                         <*> HsJSONPB.asProxy declare_response
                         <*> HsJSONPB.asProxy declare_user_info
               Hs.return
                 (HsJSONPB.NamedSchema{HsJSONPB._namedSchemaName =
                                         Hs.Just "HttpLog",
                                       HsJSONPB._namedSchemaSchema =
                                         Hs.mempty{HsJSONPB._schemaParamSchema =
                                                     Hs.mempty{HsJSONPB._paramSchemaType =
                                                                 Hs.Just HsJSONPB.SwaggerObject},
                                                   HsJSONPB._schemaProperties =
                                                     HsJSONPB.insOrdFromList
                                                       [("timestamp_ms", httpLogTimestampMs),
                                                        ("request", httpLogRequest),
                                                        ("response", httpLogResponse),
                                                        ("user_info", httpLogUserInfo)]}})
