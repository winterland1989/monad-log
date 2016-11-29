module Control.Monad.Log.IO where

import Control.Monad.Log.Base
import

type IOLogger = Logger IO

-- | make a 'Logger' based on 'FastLogger'.
makeLog :: (Level -> FormattedTime -> SrcInfo -> Text -> LogStr)  -- ^ formatter function
        -> TimeFormat                                         -- ^ check "System.Logger.FastLogger.Date"
        -> Level                                              -- ^ filter level
        -> IO Logger
makeLog fmt tfmt typ fltr env = liftIO $ do
    tc <- newTimeCache tfmt
    (fl, cl) <- newFastLogger typ
    return $ Logger fltr env fmt tc fl cl

-- | a default formatter with following format:
--
-- @[LEVEL][TIME][LOC] LOG MESSAGE\\n@
defaultFmt :: Level -> FormattedTime -> SrcInfo -> Text -> LogStr
defaultFmt lvl time env msg = toLogStr . T.concat $
    [ "[" , showt lvl, "] [", T.decodeUtf8 time,  "] [",  showt env, "] " , msg , "\n" ]

-- | a default JSON formatter with following format:
--
-- @{"level": \"LEVEL\", "time": \"TIME\", "srcInfo": \"ENV\", "msg": \"LOG MESSAGE\" }\\n@
defaultJSONFmt :: Level -> FormattedTime -> SrcInfo -> Text -> LogStr
defaultJSONFmt lvl time env msg = toLogStr . BB.toLazyByteString $
    ( fromEncoding . JSON.pairs $
        "level" .= showt lvl
        <> "time" .=  T.decodeUtf8 time
        <> "env" .= env
        <> "msg" .= msg
    ) <> "\n"

-----------------------------------------------------------------------------------------


