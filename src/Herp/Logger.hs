{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}

module Herp.Logger
    ( (.=)
    , Logger(..)
    , HasLogger(..)
    , LogLevel(..)
    , new
    , logM
    , logIO
    , recordLog
    , urgentLog
    -- * monad-logger
    , runLoggingT
    ) where

import "base" Prelude hiding (log)
import "base" Data.List qualified as List

#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.KeyMap as HashMap
#else
import "unordered-containers" Data.HashMap.Strict qualified as HashMap
#endif
import "base" Control.Concurrent ( forkIO, killThread, forkFinally )
import "base" Control.Monad (forever, forM, forM_, when)
import "base" Control.Monad.IO.Class (MonadIO(liftIO))
import "stm" Control.Concurrent.STM
import Control.Monad.Logger qualified as ML
import "base" System.IO (hSetBuffering, BufferMode(..), stdout)
import "mtl" Control.Monad.Reader (ReaderT, ask, asks, MonadReader)
import "aeson" Data.Aeson                     ((.=), Object, Value(..))
import "aeson" Data.Aeson qualified as A
import "aeson" Data.Aeson.Encoding qualified as A
import "aeson" Data.Aeson.Types               (Pair)
import "bytestring" Data.ByteString.Lazy.Char8 qualified as BC
import "bytestring" Data.ByteString.Short qualified as BS
import "fast-logger" System.Log.FastLogger.Date (newTimeCache)
import "fast-logger" System.Log.FastLogger.Types (FormattedTime, TimeFormat)
import "text" Data.Text.Encoding qualified as T
import Data.UnixTime (formatUnixTime, fromEpochTime)
import System.PosixCompat.Time (epochTime)
import "proto3-suite" Proto3.Suite.JSONPB qualified as JSONPB
import RIO (RIO(..))

import Herp.Logger.LogLevel          as X
import Herp.Logger.Transport         as X
import Herp.Util.ThreadPool

import "text" Data.Text as Text
import "text" Data.Text.Encoding as Text
import "base" GHC.Generics
import "safe-exceptions" Control.Exception.Safe qualified as E

-- $setup
-- >>> import Data.Time (getCurrentTime)
-- >>> import Text.Read (read)

data Logger = Logger
    { loggerThresholdLevel :: LogLevel
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
            <> "date" .= T.decodeUtf8 date
            <> "message" .= msg
    let series' = A.pairs $ case mextra of
                Just extra ->  series <> A.pair "extra" extra
                Nothing -> series
    BC.putStrLn $ A.encodingToLazyByteString series'

new :: ConcurrencyLevel -> LogLevel -> [Transport] -> IO Logger
new concLevel loggerThresholdLevel transports = do
    timeCache <- newTimeCache timestampFormat

    -- Initializing transports
    tids <- forM transports \Transport {name, runTransport, threshold = transportThreshold} -> do
        queue <- newTQueueIO
        thPool <-
            mkThreadPool
                concLevel
                (\e -> urgentLog Error "Exception occurred in runTransport" $
                        Just $ A.pairs (
                           "exception" .= E.displayException e
                        <> "transport" .= name
                        ))

        -- まだ書き込まれていないログを本スレッドで処理する
        let drain _ = atomically (flushTQueue queue) >>= mapM_ runTransport

        -- FIXME: Multi transports writing to the same file occurs duplicated characters like "mmeessssaaggee"
        tid <- flip forkFinally drain . forever
            $ E.bracketOnError
                (atomically $ readTQueue queue)
                (atomically . unGetTQueue queue) -- 異常終了したらdrainに流す
                (runTask thPool . runTransport) -- 同時実行数がConcurrencyLevelに達するとブロックする

        let write input = do
                let TransportInput {level = messageLevel} = input
                when (transportThreshold <= messageLevel) $ writeTQueue queue input
        pure (tid, thPool, write)

    let loggerCleanup = forM_ tids $ \(tid, thPool, _) -> do
            killThread tid
            killAllThreads thPool

    let loggerFlush = forM_ transports $ liftIO . X.flush

    let logger =
            Logger
                { loggerThresholdLevel
                , transports
                , timeCache
                , push = \input -> forM_ tids $ \(_, _, write) -> atomically $ write input
                , loggerCleanup
                , loggerFlush
                }

    let transportToValue tr = A.object ["name" .= name tr, "transport_level" .= threshold tr]
    logIO logger Informational "logger.new" $ HashMap.fromList
            [ ("log_level", A.toJSON loggerThresholdLevel)
            , ("transports", A.toJSON $ fmap transportToValue transports)
            ]
    pure logger

checkToLog :: Logger -> LogLevel -> Bool
checkToLog logger msgLevel =
    let Logger { loggerThresholdLevel } = logger
    in loggerThresholdLevel <= msgLevel

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
    when (checkToLog logger msgLevel) $ do
        push TransportInput
            { level = msgLevel
            , date = BS.toShort date
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
    let msgLevel = Informational -- datadogはloglevelを要求する
    let Logger {push, timeCache} = logger
    when (checkToLog logger msgLevel) $ do
        let options =
                JSONPB.jsonPBOptions
                    { -- NOTE: BigQueryで扱いやすくするため、デフォルト値を省略せず、oneofは使わない
                      JSONPB.optEmitDefaultValuedFields = True,
                      JSONPB.optEmitNamedOneof = False
                    }
        let value = JSONPB.toJSONPB serviceLog options
        date <- liftIO timeCache
        liftIO $ push TransportInput {
              level = msgLevel
            , date = BS.toShort date
            , message
            , extra = Just (serviceLogKey, value)
            }

serviceLogKey :: Text
serviceLogKey = "service"

runLoggingT :: Logger -> ML.LoggingT IO () -> IO ()
runLoggingT logger (ML.LoggingT run) = run $ \loc logSrc lv logStr -> do
  let msg = Text.decodeUtf8 $ ML.fromLogStr $ ML.defaultLogStr loc logSrc lv logStr
  lv <- cnvlv lv
  logIO logger lv msg mempty where
    cnvlv ML.LevelDebug = return Debug
    cnvlv ML.LevelInfo = return Informational
    cnvlv ML.LevelWarn = return Warning
    cnvlv ML.LevelError = return Error
    cnvlv (ML.LevelOther text)
      | text ~= "Notice" = return Notice
      | text ~= "Critical" = return Critical
      | text ~= "Alert" = return Alert
      | text ~= "Emergency" = return Emergency
      | otherwise = do
        let msg = "A data constructor `LevelOther Text` of type of Control.Monad.Logger.LogLevel "
                    <> "was captured while `loggerWrapper` is subscribing logs LaunchDarkly Haskell SDK "
                    <> "outputs. The argument of the constructor says: "
                    <> text
        logIO logger Notice msg mempty
        return Warning
    (~=) s t = Text.toCaseFold s == Text.toCaseFold t
