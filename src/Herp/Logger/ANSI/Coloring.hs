module Herp.Logger.ANSI.Coloring (
    module Herp.Logger.ANSI.Coloring,
    module System.Console.ANSI
)
    where

import Herp.Logger.LogLevel (LogLevel (..))
import System.Console.ANSI

-- import System.Console.ANSIhttps://hackage.haskell.org/package/ansi-terminal-0.11.3/docs/System-Console-ANSI-Types.html#t:SGR
data SGRLogInfo = NotColored | SGRLogInfo SGR

logLevelToColoredLogInfo :: LogLevel -> SGRLogInfo
logLevelToColoredLogInfo level = case level of
    Debug -> SGRLogInfo $ SetColor Foreground Dull Blue
    Informational -> NotColored
    Notice -> SGRLogInfo $ SetColor Foreground Dull Blue
    Warning -> SGRLogInfo $ SetColor Foreground Dull Magenta
    Error -> SGRLogInfo $ SetColor Foreground Dull Red
    Critical -> SGRLogInfo $ SetColor Foreground Dull Red
    Alert -> SGRLogInfo $ SetColor Foreground Dull Red
    Emergency -> SGRLogInfo $ SetColor Foreground Dull Red

setLogColor :: LogLevel -> IO ()
setLogColor level  = do
    let sgrLogInfo = logLevelToColoredLogInfo level
    case sgrLogInfo of
        NotColored -> return ()
        SGRLogInfo sgr ->
            setSGR [sgr]

resetLogColor :: IO ()
resetLogColor = setSGR [Reset]