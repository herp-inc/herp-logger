{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Herp.Logger as Logger
import System.Environment

run :: Bool -> Bool -> IO ()
run doFlush doCleanup = do
    logger <- newLogger defaultLoggerConfig
    logIO logger [ #info, "before flush", "doFlush" .= doFlush, "doCleanup" .= doCleanup ]
    when doFlush $ loggerFlush logger
    logIO logger [ #warn, "after flush", "doFlush" .= doFlush, "doCleanup" .= doCleanup ]
    when doCleanup $ loggerCleanup logger

main :: IO ()
main = getArgs >>= \case
    ["flush"] -> run True False
    ["cleanup"] -> run False True
    _ -> run False False