module Herp.Logger.ANSI.Coloring where

import Data.Colour.SRGB
import Data.String
import Herp.Logger.LogLevel (LogLevel (..), convertLogLevelToStr)
import System.Console.ANSI
    ( setSGRCode,
      ConsoleIntensity(..),
      ConsoleLayer(..),
      SGR(..) )
import qualified Data.ByteString.Lazy.Char8 as B
import Herp.Logger.Transport.Types ( TransportInput(..) )
import qualified Data.ByteString.Short as SB
import qualified Data.Aeson as A
import Text.Pretty.Simple
    ( ColorOptions(..),
      pStringOpt,
      defaultColorOptionsDarkBg,
      defaultOutputOptionsDarkBg,
      OutputOptions(..),
      Color(..),
      Intensity(..) )
import Text.Pretty.Simple.Internal.Color (colorBold)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Int (Int64)
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy as T
import qualified Data.Char as Char

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

resetSGRStr :: IsString a => a
resetSGRStr = fromString (setSGRCode [Reset])

colorBar :: (Semigroup a, IsString a) => LogLevel -> a
colorBar lv = setLogColorStr lv <> " " <> resetSGRStr <> " "

insertPrefix :: ByteString -> ByteString -> ByteString
insertPrefix prefixStr str = B.intercalate "\n" $ fmap (prefixStr <>) (B.lines str)

insertIndent :: Int64 -> ByteString -> ByteString
insertIndent n = insertPrefix (B.replicate n ' ')

insertColorBarIndent :: LogLevel -> ByteString -> ByteString
insertColorBarIndent lv = insertPrefix (colorBar lv)

toBold :: (Monoid a, IsString a) => a -> a
toBold x = mconcat [fromString $ setSGRCode [SetConsoleIntensity BoldIntensity], x, resetSGRStr]

encloseBracket :: (Semigroup a, IsString a) => a -> a
encloseBracket x = "[" <> x <> "]"

convertTransportInputToFormattedMessageANSI :: TransportInput -> ByteString
convertTransportInputToFormattedMessageANSI TransportInput{message, date, level, extra} =
        let msg = mconcat
                    [ encloseBracket . B.fromStrict . SB.fromShort $ date
                    , encloseBracket . toBold . B.pack . capitalized . convertLogLevelToStr $ level
                    , " "
                    , T.encodeUtf8 . T.fromStrict $ message
                    ]
            enc = A.encode extra
            opt = defaultOutputOptionsDarkBg
                { outputOptionsIndentAmount = 2
                , outputOptionsColorOptions = Just colorOpt
                }
            objStr = T.encodeUtf8 $ pStringOpt opt $ T.unpack $ T.decodeUtf8 enc
        in case enc of
            "{}" -> insertColorBarIndent level msg
            _ -> insertColorBarIndent level $ msg <> "\n" <> insertIndent 2 objStr
    where
        colorOpt = defaultColorOptionsDarkBg { colorString = colorBold Vivid Cyan }
        capitalized (x:xs) = Char.toUpper x : xs
        capitalized [] = []
