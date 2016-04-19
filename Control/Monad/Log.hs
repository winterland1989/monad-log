{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Control.Monad.Log (
      Level(..)
    , showTextLevel
    , NameSpace
    , LogFormatter
    , defaultFormatter
    , defaultJSONFormatter
    , Logger(..)
    , MonadLog(..)
    ) where

import System.Log.FastLogger
import Control.Monad (when)
import Control.Monad.Catch (MonadMask, finally)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Control.Exception.Lifted as Lifted
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB

import qualified Data.Aeson as JSON
import Data.Aeson ((.=))
import Data.Monoid ((<>))

-----------------------------------------------------------------------------------------

data Level
    = DEBUG                   -- ^ Debug messages
    | INFO                    -- ^ Information
    | WARNING                 -- ^ General Warnings
    | ERROR                   -- ^ General Errors
    | CRITICAL                -- ^ Severe situations
    | OTHER Text
  deriving (Eq, Ord, Show, Read)

showTextLevel :: Level -> Text
showTextLevel DEBUG        = "DEBUG"
showTextLevel INFO         = "INFO"
showTextLevel WARNING      = "WARNING"
showTextLevel ERROR        = "ERROR"
showTextLevel CRITICAL     = "CRITICAL"
showTextLevel (OTHER t)    = t

type NameSpace = Text
type LogFormatter = Level -> FormattedTime -> [NameSpace] -> Text -> LogStr

defaultFormatter :: LogFormatter
defaultFormatter lvl time ns msg = toLogStr . T.concat $
    [ "[" , showTextLevel lvl, "] [", T.decodeUtf8 time, T.intercalate ">>" ns, "] ", msg , "\n" ]

defaultJSONFormatter :: LogFormatter
defaultJSONFormatter lvl time ns msg = toLogStr . BB.toLazyByteString $
    ( JSON.fromEncoding . JSON.pairs $
        "level" .= showTextLevel lvl
        <> "time" .=  T.decodeUtf8 time
        <> "ns" .= ns
        <> "msg" .= msg
    ) <> "\n"

data Logger = Logger {
        filterLevel  :: Level
    ,   nameSpace    :: [NameSpace]
    ,   formatter    :: LogFormatter
    ,   timeCache    :: IO FormattedTime
    ,   writer       :: FastLogger
    ,   cleanUp      :: IO ()
    }

-----------------------------------------------------------------------------------------

class (MonadIO m) => MonadLog m where
    askLogger :: m Logger
    localLogger :: (Logger -> Logger) -> m a -> m a

subNameSpace :: (MonadLog m) => NameSpace -> m a -> m a
subNameSpace ns = localLogger (\ logger -> logger{ nameSpace = nameSpace logger ++ [ns] })

setFilterLevel :: (MonadLog m) => Level -> m a -> m a
setFilterLevel level = localLogger (\ logger -> logger{ filterLevel = level})

-----------------------------------------------------------------------------------------

instance (MonadIO m) => MonadLog (ReaderT Logger m) where
    askLogger = ask
    localLogger = local

makeLogger :: (MonadIO m) => LogType -> Level -> [NameSpace] -> LogFormatter -> m Logger
makeLogger typ fltr ns fmt = liftIO $ do
    tc <- newTimeCache simpleTimeFormat
    (fl, cleanUp) <- newFastLogger typ
    return $ Logger fltr ns fmt tc fl cleanUp

runLogger :: (MonadIO m, MonadMask m) => Logger -> ReaderT Logger m a -> m a
runLogger logger m = finally (runReaderT m logger) (liftIO $ cleanUp logger)

runLogger' :: (MonadBaseControl IO m, MonadIO m) => Logger -> ReaderT Logger m a -> m a
runLogger' logger m = Lifted.finally (runReaderT m logger) (liftIO $ cleanUp logger)

-----------------------------------------------------------------------------------------

logIO :: (MonadIO m) => Logger -> Text -> m ()
logIO (Logger level ns fmt tc wrt _) msg = liftIO $ tc >>= \ t -> (wrt . toLogStr) (fmt level t ns msg)
{-# INLINE logIO #-}

info :: (MonadLog m) => Text -> m ()
info msg = do
    logger <- askLogger
    when (filterLevel logger <= INFO) (logIO logger msg)
