{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad.Reader
import Herp.Logger as Logger
import Herp.Logger.Transport.Stdout
import System.Log.FastLogger.LoggerSet as LS

loggerLevelTest config = do
    logger <- Logger.newLogger config

    flip runReaderT logger $ do
        logM [ #debug, "debug" ]
        logM [ #info, "hello, world", "key" .= ("value" :: String) ]
        logM [ #notice, "lorem ipsum" ]
        logM [ #warn, "lorem ipsum" ]
        logM [ #error, "lorem ipsum" ]
        logM [ #crit, "lorem ipsum" ]
        logM [ #alert, "lorem ipsum" ]
        logM [ #emerg, "lorem ipsum" ]
        let msg = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Duis tortor enim, facilisis vitae ex non, molestie fringilla dui. Duis porta neque risus, eu iaculis odio semper quis. Cras suscipit molestie lacus, ac fringilla nulla blandit quis. Maecenas feugiat erat neque, id mattis nibh elementum sed. Donec nec felis nisi. Cras facilisis dui imperdiet velit rhoncus lobortis. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Praesent eget lorem consequat, elementum sapien eu, rhoncus est. Etiam tincidunt leo nibh, a ullamcorper orci tincidunt id. Curabitur non dui ac ipsum eleifend iaculis ultrices at felis.\n\nAenean mauris metus, iaculis eget sagittis ut, fringilla nec nunc. Vivamus ante dolor, bibendum ut tellus gravida, ultricies bibendum mi. Morbi vel lectus pulvinar, accumsan leo eu, maximus ante. Morbi justo nisl, malesuada eu ex venenatis, bibendum aliquam arcu. In scelerisque elementum eros, id pretium libero eleifend ac. Quisque dictum, turpis a faucibus tempus, tellus leo ullamcorper libero, ac condimentum ex ante eu tortor. Sed sit amet dolor arcu."
        logM [ #info, msg, "key" .= ("value" :: String) ]

    loggerCleanup logger

main :: IO ()
main = bracket (LS.newStdoutLoggerSet 4096) LS.rmLoggerSet $ \loggerSet -> do
    loggerLevelTest defaultLoggerConfig
    loggerLevelTest defaultLoggerConfig{createTransports = pure [stdoutTransport loggerSet Debug]}
    loggerLevelTest defaultLoggerConfig{createTransports = pure [stdoutTransport loggerSet Debug], logLevel = Informational}
    loggerLevelTest defaultLoggerConfig{createTransports = pure [stdoutANSITransport loggerSet Debug]}
