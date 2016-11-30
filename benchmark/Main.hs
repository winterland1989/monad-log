module Main where

-------------------------------------------------------------------------------

import           Criterion.Main
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import     Data.Time
import qualified Data.Thyme as Y
import qualified System.Locale as Y
main :: IO ()
main = do
    defaultMain
        [ bgroup "time vs unixtime"
            [ bench "time" $ nfIO Y.getCurrentTime
            , bench "thyme" $ nfIO Y.getZonedTime
            ]
        ]

