{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module provides a logger parametrized by monad m, it's useful
-- when you want to test your application with a custom base monad.
--
module Control.Monad.Log.Base (
    -- * parametrized 'Logger'
      Level(..)
    , SrcInfo, FormattedTime, Message
    , Logger(..)
    , defaultFormatter
    -- * logging functions
    , logAt
    , debug
    , info
    , warn
    , err
    , fatal
    -- * re-export from text-show and fast-logger
    ) where

import Control.Monad (when)
import Control.Monad.Reader.Class
import Control.Monad.Base
import Data.Has
import Data.Text (Text)
import qualified Data.Text as T

-----------------------------------------------------------------------------------------

-- | Logger level datatype.
--
newtype Level = Level Int deriving (Eq, Ord, Show, Read)

pattern DEBUG = Level 0
pattern INFO  = Level 1
pattern WARN  = Level 2
pattern ERR   = Level 3
pattern FATAL = Level 4

levelToText :: Level -> Text
levelToText DEBUG     = "DEBUG"
levelToText INFO      = "INFO"
levelToText WARN      = "WARN"
levelToText ERR       = "ERR"
levelToText FATAL     = "FATAL"

type SrcInfo = Text
type FormattedTime = Text
type Message = Text

-- | Logger configurations.
data Logger m = Logger {
        filterLevel  :: Level  -- ^ filter level, equal or above it will be logged.
    ,   formatter    :: Level -> FormattedTime -> SrcInfo -> Message -> Text -- ^ formatter function.
    ,   timeCache    :: m FormattedTime     -- ^ get time string
    ,   writeLog     :: Text -> m ()        -- ^ log in Monad m
    ,   flushLog     :: m ()                -- ^ flush log
    }

-- | A default formatter with following format:
--
-- @[LEVEL][TIME][SRCINFO] MESSAGE\\n@
--
defaultFormatter :: Level -> FormattedTime -> SrcInfo -> Text -> Text
defaultFormatter lvl time srcInfo msg = T.concat $
    [ "[" , levelToText lvl, "][", time,  "][", srcInfo, "] ", msg, "\n" ]

-----------------------------------------------------------------------------------------

logAt :: (Has (Logger b) r, MonadBase b m, MonadReader r m) => Level -> SrcInfo -> Message -> m ()
logAt lvl srcInfo msg = do
    Logger{..} <- asks getter
    when (lvl >= filterLevel) $ liftBase $
        timeCache >>= \ t -> writeLog (formatter lvl t srcInfo msg)
{-# INLINE logAt #-}

debug :: (Has (Logger b) r, MonadBase b m, MonadReader r m) => SrcInfo -> Message -> m ()
debug = logAt DEBUG

info :: (Has (Logger b) r, MonadBase b m, MonadReader r m) => SrcInfo -> Message -> m ()
info = logAt INFO

warn :: (Has (Logger b) r, MonadBase b m, MonadReader r m) => SrcInfo -> Message -> m ()
warn = logAt WARN

err :: (Has (Logger b) r, MonadBase b m, MonadReader r m) => SrcInfo -> Message -> m ()
err = logAt ERR

fatal :: (Has (Logger b) r, MonadBase b m, MonadReader r m) => SrcInfo -> Message -> m ()
fatal = logAt FATAL
