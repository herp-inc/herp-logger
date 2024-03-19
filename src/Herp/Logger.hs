{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}

module Herp.Logger
    ( (.=)
    , Logger(..)
    , HasLogger(..)
    , LogLevel(..)
    , LoggerConfig(..)
    , ConcurrencyLevel
    , newLogger
    , withLogger
    , defaultLoggerConfig
    , logM
    , logOtherM
    , logDebugM
    , logInfoM
    , logNoticeM
    , logWarnM
    , logErrorM
    , logCritM
    , logAlertM
    , logEmergM
    , logIO
    , urgentLog
    , Herp.Logger.flush
    -- * Payload
    , Payload
    , P.level
    , P.message
    , P.object
    , P.messageString
    , P.messageShow
    , P.messageException
    -- * monad-logger
    , runLoggingT
    , toLoggerIO
    -- * Hooks
    , Hooks(..)
    , defaultHooks
    ) where

import "base" Prelude hiding (log)
import "base" Data.Semigroup (Max(..))

import "base" Control.Concurrent ( killThread, forkFinally )
import "async" Control.Concurrent.Async (async, Async, cancel)
import "base" Control.Monad (forever, forM, forM_, when)
import "base" Control.Monad.IO.Class (MonadIO(liftIO))
import "stm" Control.Concurrent.STM
import Control.Monad.Logger qualified as ML
import "base" System.IO (hSetBuffering, BufferMode(..), stdout, hIsTerminalDevice)
import "mtl" Control.Monad.Reader (asks, MonadReader)
import "aeson" Data.Aeson                     ((.=))
import "aeson" Data.Aeson qualified as A
import "aeson" Data.Aeson.Encoding qualified as A
import "bytestring" Data.ByteString.Lazy.Char8 qualified as BC
import "bytestring" Data.ByteString.Short qualified as BS
import "fast-logger" System.Log.FastLogger.Date (newTimeCache)
import "fast-logger" System.Log.FastLogger.LoggerSet
import "fast-logger" System.Log.FastLogger.Types (FormattedTime, TimeFormat)
import "resource-pool" Data.Pool
import "text" Data.Text.Encoding qualified as T
import Data.UnixTime (formatUnixTime, fromEpochTime)
import System.PosixCompat.Time (epochTime)
import "vector" Data.Vector (Vector)

import Herp.Logger.Payload           as P
import Herp.Logger.LogLevel          as X
import Herp.Logger.Transport.Types   as X
import Herp.Logger.Transport.Stdout  as X

import "text" Data.Text as Text
import "text" Data.Text.Encoding as Text
import "base" GHC.Generics
import "safe-exceptions" Control.Exception.Safe qualified as E


type ThreadPool = Pool (TQueue (IO ()), Async ())

type ConcurrencyLevel = Int

mkThreadPool :: ConcurrencyLevel -> (E.SomeException -> IO ()) -> IO ThreadPool
mkThreadPool poolMaxResources errHandler = do
    let createResource = do
            queue <- newTQueueIO
            thread <- async . forever $ do
                E.bracketOnError
                    (atomically $ readTQueue queue)
                    (atomically . unGetTQueue queue)
                    id `E.catch` (\(e :: E.SomeException) -> errHandler e)
            pure (queue, thread)
    let freeResource (queue, thread) = do
            atomically (flushTQueue queue) >>= sequence_
            cancel thread
    let poolCacheTTL = 3600
    pool <- newPool $ setNumStripes (Just 1) $ defaultPoolConfig createResource freeResource poolCacheTTL poolMaxResources
    pure pool

-- NOTE: It *blocks* when there are no threads in idle.
runTask :: ThreadPool -> IO () -> IO ()
runTask thPool param = withResource thPool $ \ (ch, _th) -> atomically (writeTQueue ch param)

-- $setup
-- >>> import Data.Time (getCurrentTime)
-- >>> import Text.Read (read)

data Logger = Logger
    { loggerThresholdLevel :: LogLevel
    , transports :: Vector Transport
    , timeCache :: IO FormattedTime
    , push :: TransportInput -> IO ()
    , loggerCleanup :: IO ()
    , loggerFlush :: IO ()
    , loggerHooks :: Hooks
    }
    deriving stock Generic

timestampFormat :: TimeFormat
timestampFormat = "%Y-%m-%dT%H:%M:%S%z"

-- | `urgentLog` outputs log to stdout without logger.
--   It's used when logger is busy, not created or in trouble.
urgentLog :: LogLevel -> Text -> A.Series -> IO ()
urgentLog msgLevel msg extra = do
    hSetBuffering stdout LineBuffering
    now <- epochTime
    date <- formatUnixTime timestampFormat $ fromEpochTime now
    let series =
               "level" .= msgLevel
            <> "date" .= T.decodeUtf8 date
            <> "message" .= msg
    BC.putStrLn $ A.encodingToLazyByteString $ A.pairs $ series <> extra

data LoggerConfig = LoggerConfig
    { createTransports :: IO (Vector Transport)
    , concurrencyLevel :: ConcurrencyLevel
    , logLevel :: LogLevel
    , hooks :: Hooks
    }

defaultLoggerConfig :: LoggerConfig
defaultLoggerConfig =
    LoggerConfig
        { createTransports = do
            ls <- newStdoutLoggerSet 4096
            istty <-  hIsTerminalDevice stdout
            if istty
                then pure [stdoutANSITransport ls Debug]
                else pure [stdoutTransport ls Debug]
        , concurrencyLevel = 1
        , logLevel = Debug
        , hooks = defaultHooks
        }

-- 将来別のフックを追加したくなるかもしれないので Hooks でくるんでいる
data Hooks =
    Hooks
        { logHook :: (Logger -> Payload -> IO ()) -> Logger -> Payload -> IO ()
        }

defaultHooks :: Hooks
defaultHooks = Hooks id

withLogger :: LoggerConfig -> (Logger -> IO a) -> IO a
withLogger config = E.bracket (newLogger config) loggerCleanup

newLogger :: LoggerConfig -> IO Logger
newLogger LoggerConfig{..} = do
    timeCache <- newTimeCache timestampFormat
    transports <- createTransports
    -- Initializing transports
    tids <- forM transports \Transport {name, runTransport, threshold = transportThreshold} -> do
        queue <- newTQueueIO
        thPool <-
            mkThreadPool
                concurrencyLevel
                (\e -> urgentLog Error "Exception occurred in runTransport" $
                           "exception" .= E.displayException e
                        <> "transport" .= name)

        -- まだ書き込まれていないログを本スレッドで処理する
        let drain _ = atomically (flushTQueue queue) >>= mapM_ runTransport

        tid <- flip forkFinally drain . forever
            $ E.bracketOnError
                (atomically $ readTQueue queue)
                (atomically . unGetTQueue queue) -- 異常終了したらdrainに流す
                (runTask thPool . runTransport) -- 同時実行数がConcurrencyLevelに達するとブロックする

        let write input = do
                let TransportInput {level = messageLevel} = input
                when (transportThreshold <= messageLevel) $ writeTQueue queue input
        pure (tid, thPool, write)

    let loggerFlush = do
            forM_ tids $ \(_, thPool, _) -> destroyAllResources thPool
            forM_ transports $ liftIO . X.flush

    let loggerCleanup = do
            loggerFlush
            forM_ tids $ \(tid, thPool, _) -> do
                killThread tid
                destroyAllResources thPool

    let logger =
            Logger
                { loggerThresholdLevel = logLevel
                , transports = transports
                , timeCache
                , push = \input -> forM_ tids $ \(_, _, write) -> atomically $ write input
                , loggerCleanup
                , loggerFlush
                , loggerHooks = hooks
                }

    let transportToValue tr = A.object ["name" .= name tr, "transport_level" .= threshold tr]
    logIO logger
            [ #info
            , "logger.new"
            , "log_level" .= logLevel
            , "transports" .= fmap transportToValue transports
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
log' :: Logger -> FormattedTime -> Payload -> IO ()
log' logger date obj = do
    let Logger { push } = logger
    let lvl = getMax $ payloadLevel obj
    when (checkToLog logger lvl) $ do
        push TransportInput
            { level = lvl
            , date = BS.toShort date
            , message = payloadMessage obj
            , extra = payloadObject obj
            }

class HasLogger a where
    toLogger :: a -> Logger

instance HasLogger Logger where
    toLogger = id

logIO :: MonadIO m => Logger -> Payload -> m ()
logIO logger@Logger {timeCache} msg = do
    date <- liftIO timeCache
    liftIO $ logHook (loggerHooks logger) (flip log' date) logger msg
{-# INLINE logIO #-}

logM :: forall r m. (MonadReader r m, HasLogger r, MonadIO m) => Payload -> m ()
logM msg = do
    logger <- asks toLogger
    logIO logger msg
{-# INLINE logM #-}

logOtherM :: (MonadReader r m, HasLogger r, MonadIO m) => LogLevel -> Payload -> m ()
logOtherM logLevel payload = logM $ P.level logLevel <> payload

logDebugM :: (MonadReader r m, HasLogger r, MonadIO m) => Payload -> m ()
logDebugM = logOtherM Debug

logInfoM :: (MonadReader r m, HasLogger r, MonadIO m) => Payload -> m ()
logInfoM = logOtherM Informational

logNoticeM :: (MonadReader r m, HasLogger r, MonadIO m) => Payload -> m ()
logNoticeM = logOtherM Notice

logWarnM :: (MonadReader r m, HasLogger r, MonadIO m) => Payload -> m ()
logWarnM = logOtherM Warning

logErrorM :: (MonadReader r m, HasLogger r, MonadIO m) => Payload -> m ()
logErrorM = logOtherM Error

logCritM :: (MonadReader r m, HasLogger r, MonadIO m) => Payload -> m ()
logCritM = logOtherM Critical

logAlertM :: (MonadReader r m, HasLogger r, MonadIO m) => Payload -> m ()
logAlertM = logOtherM Alert

logEmergM :: (MonadReader r m, HasLogger r, MonadIO m) => Payload -> m ()
logEmergM = logOtherM Emergency

flush :: forall r m. (MonadReader r m, HasLogger r, MonadIO m) => m ()
flush = asks toLogger >>= liftIO . loggerFlush

runLoggingT :: forall m a. Logger -> ML.LoggingT m a -> m a
runLoggingT logger (ML.LoggingT run) = run (toLoggerIO logger)

-- | Convert a 'Logger' to one that's compabible with monad-logger
toLoggerIO :: Logger -> ML.Loc -> ML.LogSource -> ML.LogLevel -> ML.LogStr -> IO ()
toLoggerIO logger loc logSrc lv logStr = do
  let msg = Text.decodeUtf8 $ ML.fromLogStr $ ML.defaultLogStr loc logSrc lv logStr
  logIO logger
    [ P.message msg
    , case convertLogLevel lv of
        Right x -> P.level x
        Left other -> [#warn, "level" .= other]
    ]
