module Herp.Logger.ANSI.Coloring where

import Data.Colour.SRGB
import Data.String
import Herp.Logger.LogLevel (LogLevel (..))
import System.Console.ANSI

-- https://hackage.haskell.org/package/ansi-terminal-0.11.3/docs/System-Console-ANSI-Types.html#t:SGR

logLevelToSGR :: LogLevel -> [SGR]
logLevelToSGR level = case level of
    Debug -> [SetRGBColor Background $ sRGB24 196 196 196] -- Grey
    Informational -> [SetRGBColor Background $ sRGB24 155 204 228] -- Light blue
    Notice -> [SetRGBColor Background $ sRGB24 154 169 186] -- Grey
    Warning -> [SetRGBColor Background $ sRGB24 237 179 89] -- Orange
    Error -> [SetRGBColor Background $ sRGB24 226 86 77] -- Red
    Critical -> [SetRGBColor Background $ sRGB24 202 9 10] -- Wine red
    Alert -> [SetRGBColor Background $ sRGB24 152 0 0] -- #980000
    Emergency -> [SetRGBColor Background $ sRGB24 101 0 0] --- #650000

setLogColorStr :: IsString a => LogLevel -> a
setLogColorStr level =
    let sgrLogInfo = logLevelToSGR level
     in fromString (setSGRCode sgrLogInfo)

resetLogColorStr :: IsString a => a
resetLogColorStr = fromString (setSGRCode [Reset])

coloringLogInfoStr :: (IsString a, Semigroup a) => LogLevel -> a -> a
coloringLogInfoStr level str = setLogColorStr level <> " " <> resetLogColorStr <> " " <> str
