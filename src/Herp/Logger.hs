{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE PackageImports #-}

module Herp.Logger
    ( (.=)
    , Logger
    , HasLogger(..)
    , LogLevel(..)
    , new
    , logM
    , logIO
    , recordLog
    , urgentLog
    ) where

import "base" Prelude hiding (log)
import "base" Data.List qualified as List
import "unordered-containers" Data.HashMap.Strict qualified as HashMap
import "base" Control.Concurrent ( forkIO, killThread)
import "base" Control.Concurrent.Chan (Chan, dupChan, newChan, readChan, writeChan)
import "base" Control.Monad (forever, forM, forM_, when)
import "base" Control.Monad.IO.Class (MonadIO(liftIO))
import "base" System.IO (hSetBuffering, BufferMode(..), stdout)
import "mtl" Control.Monad.Reader (ReaderT, ask, asks, MonadReader)
import "aeson" Data.Aeson                     ((.=), Object, Value(..))
import "aeson" Data.Aeson qualified as A
import "aeson" Data.Aeson.Encoding qualified as A
import "aeson" Data.Aeson.Types               (Pair)
import "bytestring" Data.ByteString.Lazy.Char8 qualified as BC
import "fast-logger" System.Log.FastLogger.Date (newTimeCache)
import "fast-logger" System.Log.FastLogger.Types (FormattedTime, TimeFormat)
import Data.UnixTime (formatUnixTime, fromEpochTime)
import System.PosixCompat.Time (epochTime)
import "proto3-suite" Proto3.Suite.JSONPB qualified as JSONPB
import RIO (RIO(..))

import Herp.Logger.LogLevel          as X
import Herp.Logger.Transport         as X
import Herp.Util.Lens hiding ((.=))
import Herp.Util.ThreadPool

import "text" Data.Text as Text
import "base" GHC.Generics
import "safe-exceptions" Control.Exception.Safe qualified as E

-- $setup
-- >>> import Data.Time (getCurrentTime)
-- >>> import Text.Read (read)

data Logger = Logger
    { loggerThresholdLevel :: LogLevel
    , minTransportThreshold :: LogLevel
    , transports :: [Transport]
    , timeCache :: IO FormattedTime
    , push :: TransportInput -> IO ()
    , loggerCleanup :: IO ()
    , loggerFlush :: IO ()
    }
    deriving stock Generic

timestampFormat :: TimeFormat
timestampFormat = "%Y-%m-%dT%H:%M:%S%z"

-- | `urgentLog` outputs log to stdout without logger.
--   It's used when logger is busy, not created or in trouble.
urgentLog :: LogLevel -> Text -> Maybe A.Encoding -> IO ()
urgentLog msgLevel msg mextra = do
    hSetBuffering stdout LineBuffering
    now <- epochTime
    date <- formatUnixTime timestampFormat $ fromEpochTime now
    let series =
               "level" .= msgLevel
            <> "date" .= (date ^. utf8)
            <> "message" .= msg
    let series' = A.pairs $ case mextra of
                Just extra ->  series <> A.pair "extra" extra
                Nothing -> series
    BC.putStrLn $ A.encodingToLazyByteString series'

new :: ConcurrencyLevel -> LogLevel -> [Transport] -> IO Logger
new concLevel loggerThresholdLevel transports = do
    timeCache <- newTimeCache timestampFormat

    -- Initializing transports

    (dupChans, push) <- duplicatedChans (Prelude.length transports)

    tids <- forM (Prelude.zip dupChans transports) $ \(chan', Transport {name, runTransport, threshold = transportThreshold}) -> do
        thPool <-
            mkThreadPool
                concLevel
                runTransport
                (\e -> urgentLog Error "Exception occurred in runTransport" $
                        Just $ A.pairs (
                           "exception" .= E.displayException e
                        <> "transport" .= name
                        ))

        -- FIXME: Multi transports writing to the same file occurs duplicated characters like "mmeessssaaggee"
        tid <- forkIO . forever $ do
            -- Start listening for the channel
            input <- readChan chan'
            let TransportInput {level = messageLevel} = input
            when (loggerThresholdLevel <= messageLevel && transportThreshold <= messageLevel) $
                runTask thPool input -- NOTE: It blocks when reached ConcurrencyLevel
        pure (tid, thPool)

    -- NOTE: Ensure logger stops before cleanup
    let loggerCleanup = forM_ tids $ \(tid, thPool) -> do
            killThread tid
            killAllThreads thPool

    let minTransportThreshold = List.minimum $ fmap threshold transports
    let loggerFlush = forM_ transports $ liftIO . X.flush

    let logger =
            Logger
                { loggerThresholdLevel
                , minTransportThreshold
                , transports
                , timeCache
                , push
                , loggerCleanup
                , loggerFlush
                }

    let transportToValue tr = A.object ["name" .= name tr, "transport_level" .= threshold tr]
    logIO logger Informational "logger.new" $ HashMap.fromList
            [ ("log_level", A.toJSON loggerThresholdLevel)
            , ("transports", A.toJSON $ fmap transportToValue transports)
            ]
    pure logger

-- n = 0 のときはチャンネルも無いはずなのでチャンネルに書く処理は何もしない
-- 複製後のチャンネルの数を n にしたいので複製の回数は n - 1 回
duplicatedChans :: Int -> IO ([Chan a], a -> IO ())
duplicatedChans 0  = pure ([], const $ pure ())
duplicatedChans n' = do
    chan <- newChan

    let dups 1 = pure [chan]
        dups n = (:) <$> dupChan chan <*> dups (n - 1)

    (,) <$> dups n' <*> pure (writeChan chan)

checkToLog :: Logger -> LogLevel -> Bool
checkToLog logger msgLevel =
    let Logger { loggerThresholdLevel, minTransportThreshold } = logger
    in loggerThresholdLevel <= msgLevel && minTransportThreshold <= msgLevel

-- |
--
-- >>> logger <- new Informational []
-- >>> date = "2019-08-06T10:01:05Z"
-- >>> log' logger Warning date "Hello" mempty
-- {"date":"2019-08-06T10:01:05Z","message":"Hello","level":"warn"}
log' :: Logger -> LogLevel -> FormattedTime -> Text -> Object -> IO ()
log' logger msgLevel date message ~obj = do
    let Logger { push } = logger
    let extraKey = "extra"
    -- NOTE: Check LogLevel before writing to chan in case chan gets stuck.
    when (checkToLog logger msgLevel) $ do
        push TransportInput
            { level = msgLevel
            , date = date ^. shortBS
            , message
            , extra = if HashMap.null obj then Nothing else Just (extraKey, Object obj)
            }

class HasLogger a where
    toLogger :: a -> Logger

instance HasLogger Logger where
    toLogger = id

logIO :: MonadIO m => Logger -> LogLevel -> Text
    -> Object -- ^ extra data
    -> m ()
logIO logger@Logger {timeCache} msgLevel ~msg ~obj = do
    date <- liftIO timeCache
    liftIO $ log' logger msgLevel date msg obj
{-# SPECIALIZE logIO :: Logger -> LogLevel -> Text -> Object -> IO () #-}

logM :: forall r m. (MonadReader r m, HasLogger r, MonadIO m) => LogLevel -> Text
    -> Object -- ^ extra data
    -> m ()
logM msgLevel ~msg ~obj = do
    logger <- asks toLogger
    logIO logger msgLevel msg obj

flush :: forall r m. (MonadReader r m, HasLogger r, MonadIO m) => m ()
flush = asks toLogger >>= liftIO . loggerFlush

-- logging function for service log
recordLog :: (MonadIO m, JSONPB.ToJSONPB serviceLog) => Logger -> Text -> serviceLog -> m ()
recordLog logger message serviceLog = do
    let msgLevel = #notice -- NOTE: datadog seems to require loglevel (to classify logs?)
    let Logger {push, timeCache} = logger
    when (checkToLog logger msgLevel) $ do
        let options =
                JSONPB.jsonPBOptions
                    { -- NOTE: To store to bigquery emitting default value is preferred
                      JSONPB.optEmitDefaultValuedFields = True
                    , -- NOTE: To store to bigquery "oneof" enclosure is unnecessary
                      JSONPB.optEmitNamedOneof = False
                    }
        let value = JSONPB.toJSONPB serviceLog options
        date <- liftIO timeCache
        liftIO $ push TransportInput {
              level = msgLevel
            , date = date ^. shortBS
            , message
            , extra = Just (serviceLogKey, value)
            }

serviceLogKey :: Text
serviceLogKey = "service"
