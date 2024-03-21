{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Herp.Logger as Logger

main :: IO ()
main = do
    withLogger defaultLoggerConfig{hooks = Hooks{logHook = myLogHook}} $ \logger -> do
        logIO logger [#debug, "simple"]
        logIO logger [#debug, "override", "hook" .= ("wins" :: String) ]
        loggerFlush logger

myLogHook :: (Logger -> Payload -> IO ()) -> Logger -> Payload -> IO ()
myLogHook action logger payload =
    action logger $ payload <> [ "hook" .= ("called" :: String) ]
