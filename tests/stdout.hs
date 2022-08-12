{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad.Reader
import Herp.Logger as Logger
import Herp.Logger.StdoutTransport
import System.Log.FastLogger.LoggerSet as LS

loggerLevelTest loggerSet lv = do
    logger <- Logger.new 1 lv [stdoutTransport loggerSet Debug]

    flip runReaderT logger $ do
        logM [ #debug, "debug" ]
        logM [ #info, "hello, world", "key" .= ("value" :: String) ]
        logM [ #notice, "lorem ipsum" ]
        logM [ #warn, "lorem ipsum" ]
        logM [ #error, "lorem ipsum" ]
        logM [ #crit, "lorem ipsum" ]
        logM [ #alert, "lorem ipsum" ]
        logM [ #emerg, "lorem ipsum" ]
    
    loggerCleanup logger

main :: IO ()
main = bracket (LS.newStdoutLoggerSet 4096) LS.rmLoggerSet $ \loggerSet -> do
    loggerLevelTest loggerSet Debug
    loggerLevelTest loggerSet Informational
