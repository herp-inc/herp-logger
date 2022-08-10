module Herp.Logger.ANSI.Coloring where

import Data.Colour.SRGB
import Data.String
import Herp.Logger.LogLevel (LogLevel (..))
import System.Console.ANSI

-- https://hackage.haskell.org/package/ansi-terminal-0.11.3/docs/System-Console-ANSI-Types.html#t:SGR
data SGRLogInfo = NotColored | SGRLogInfo [SGR]

logLevelToColoredLogInfo :: LogLevel -> SGRLogInfo
logLevelToColoredLogInfo level = case level of
    Debug -> SGRLogInfo [SetRGBColor Background $ sRGB24 196 196 196] -- Grey
    Informational -> SGRLogInfo [SetRGBColor Background $ sRGB24 155 204 228] -- Light blue
    Notice -> SGRLogInfo [SetRGBColor Background $ sRGB24 154 169 186] -- Grey
    Warning -> SGRLogInfo [SetRGBColor Background $ sRGB24 237 179 89] -- Orange
    Error -> SGRLogInfo [SetRGBColor Background $ sRGB24 226 86 77] -- Red
    Critical -> SGRLogInfo [SetRGBColor Background $ sRGB24 202 9 10] -- Wine red
    Alert -> SGRLogInfo [SetRGBColor Background $ sRGB24 152 0 0] -- #980000
    Emergency -> SGRLogInfo [SetRGBColor Background $ sRGB24 101 0 0] --- #650000

setLogColorStr :: IsString a => LogLevel -> a
setLogColorStr level =
    let sgrLogInfo = logLevelToColoredLogInfo level
     in case sgrLogInfo of
            NotColored -> ""
            SGRLogInfo sgr ->
                fromString (setSGRCode sgr)

resetLogColorStr :: IsString a => a
resetLogColorStr = fromString (setSGRCode [Reset])

coloringLogInfoStr :: (IsString a, Semigroup a) => LogLevel -> a -> a
-- coloringLogInfoStr level str = setLogColorStr level <> str <> resetLogColorStr
coloringLogInfoStr level str = setLogColorStr level <> " " <> resetLogColorStr <> " " <> str
