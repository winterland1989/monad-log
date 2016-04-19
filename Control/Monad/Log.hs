{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Monad.Log (
    -- data types, defaults
      Level(..)
    , showTextLevel
    , NameSpace
    , LogFormatter
    , defaultFormatter
    , defaultFormatterExt
    , defaultJSONFormatter
    , defaultJSONFormatterExt
    , Logger(..)
    , makeLogger
    -- 'MonadLog' class
    , MonadLog(..)
    , subNameSpace
    , setFilterLevel
    -- ReaderT based 'MonadLog'
    , runLogger
    , runLogger'
    -- logging functions
    , debug
    , info
    , warning
    , error
    , critical
    , debugE
    , infoE
    , warningE
    , errorE
    , criticalE
    ) where

import Control.Monad (when)
import Control.Monad.Catch (MonadMask, finally)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Control.Exception.Lifted as Lifted
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import System.Log.FastLogger
import Prelude hiding (error)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import TextShow (TextShow, showt, showb)

import qualified Data.Aeson as JSON
import Data.Aeson (ToJSON, toJSON, toEncoding, fromEncoding, (.=))
import Data.Monoid ((<>))

import Language.Haskell.TH.Syntax (Loc(..), Q, Exp)
import qualified Language.Haskell.TH.Syntax as TH
import Control.Concurrent (ThreadId, myThreadId)

-----------------------------------------------------------------------------------------

data Level
    = DEBUG                   -- ^ Debug messages
    | INFO                    -- ^ Information
    | WARNING                 -- ^ General Warnings
    | ERROR                   -- ^ General Errors
    | CRITICAL                -- ^ Severe situations
  deriving (Eq, Ord, Show, Read)

showTextLevel :: Level -> Text
showTextLevel DEBUG        = "DEBUG"
showTextLevel INFO         = "INFO"
showTextLevel WARNING      = "WARNING"
showTextLevel ERROR        = "ERROR"
showTextLevel CRITICAL     = "CRITICAL"

type NameSpace = Text
type LogFormatter = Level -> FormattedTime -> [NameSpace] -> Text -> LogStr
type LogFormatterExt = forall a. (TextShow a, ToJSON a) => Level -> FormattedTime -> [NameSpace] -> a -> Text -> LogStr

defaultFormatter :: LogFormatter
defaultFormatter lvl time ns msg = toLogStr . T.concat $
    [ "[" , showTextLevel lvl, "] [", T.decodeUtf8 time, T.intercalate ">>" ns, "] ", msg , "\n" ]

defaultFormatterExt :: LogFormatterExt
defaultFormatterExt lvl time ns ext msg = toLogStr . T.concat $
    [ "[" , showTextLevel lvl, "] [", T.decodeUtf8 time, T.intercalate ">>" ns, "] [", showt ext, "] " , msg , "\n" ]

defaultJSONFormatter :: LogFormatter
defaultJSONFormatter lvl time ns msg = toLogStr . BB.toLazyByteString $
    ( fromEncoding . JSON.pairs $
        "level" .= showTextLevel lvl
        <> "time" .=  T.decodeUtf8 time
        <> "ns" .= ns
        <> "msg" .= msg
    ) <> "\n"

defaultJSONFormatterExt :: LogFormatterExt
defaultJSONFormatterExt lvl time ns ext msg = toLogStr . BB.toLazyByteString $
    ( JSON.fromEncoding . JSON.pairs $
        "level" .= showTextLevel lvl
        <> "time" .=  T.decodeUtf8 time
        <> "ns" .= ns
        <> "ext" .= ext
        <> "msg" .= msg
    ) <> "\n"

data Logger = Logger {
        filterLevel  :: Level                 -- ^ filter level, equal or above it will be logged.
    ,   nameSpace    :: [NameSpace]           -- ^ a list of 'Text' from root to children, see 'subNameSpace'.
    ,   formatter    :: LogFormatter          -- a 'LogFormatter' function.
    ,   formatterExt :: LogFormatterExt       -- a 'LogFormatterExt' function.
    ,   timeCache    :: IO FormattedTime      -- a time cache to avoid cost of frequently formatting time.
    ,   writer       :: FastLogger            -- the log function.
    ,   cleanUp      :: IO ()                 -- clean up action(flushing/closing...).
    }

makeLogger :: (MonadIO m) => LogType -> Level -> [NameSpace] -> LogFormatter -> LogFormatterExt -> m Logger
makeLogger typ fltr ns fmt fmtExt = liftIO $ do
    tc <- newTimeCache simpleTimeFormat
    (fl, cleanUp) <- newFastLogger typ
    return $ Logger fltr ns fmt fmtExt tc fl cleanUp

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

runLogger :: (MonadIO m, MonadMask m) => Logger -> ReaderT Logger m a -> m a
runLogger logger m = finally (runReaderT m logger) (liftIO $ cleanUp logger)

runLogger' :: (MonadBaseControl IO m, MonadIO m) => Logger -> ReaderT Logger m a -> m a
runLogger' logger m = Lifted.finally (runReaderT m logger) (liftIO $ cleanUp logger)

-----------------------------------------------------------------------------------------

log' :: (MonadLog m) => Level -> Text -> m ()
log' fltr msg = do
    (Logger lvl ns fmt _ tc wrt _) <- askLogger
    when (lvl <= fltr) $
        liftIO $ tc >>= \ t -> (wrt . toLogStr) (fmt lvl t ns msg)
{-# INLINE log' #-}

logExt :: (TextShow a, ToJSON a, MonadLog m) => Level -> a -> Text -> m ()
logExt fltr ext msg = do
    (Logger lvl ns fmt fmtExt tc wrt _) <- askLogger
    when (lvl <= fltr) $
        liftIO $ tc >>= \ t -> (wrt . toLogStr) (fmtExt lvl t ns ext msg)
{-# INLINE logExt #-}

debug :: (MonadLog m) => Text -> m ()
debug = log' DEBUG

info :: (MonadLog m) => Text -> m ()
info = log' INFO

warning :: (MonadLog m) => Text -> m ()
warning = log' WARNING

error :: (MonadLog m) => Text -> m ()
error = log' ERROR

critical :: (MonadLog m) => Text -> m ()
critical = log' CRITICAL

debugE :: (TextShow a, ToJSON a, MonadLog m) => a -> Text -> m ()
debugE = logExt DEBUG

infoE :: (TextShow a, ToJSON a, MonadLog m) => a -> Text -> m ()
infoE = logExt INFO

warningE :: (TextShow a, ToJSON a, MonadLog m) => a -> Text -> m ()
warningE = logExt WARNING

errorE :: (TextShow a, ToJSON a, MonadLog m) => a -> Text -> m ()
errorE = logExt ERROR

criticalE :: (TextShow a, ToJSON a, MonadLog m) => a -> Text -> m ()
criticalE = logExt CRITICAL

-----------------------------------------------------------------------------------------

newtype LogLoc = LogLoc Loc deriving (Show, Eq, Ord)

instance TextShow LogLoc where
    showb (LogLoc (Loc file _ _ start end)) = showb file <> showb start <> showb end

instance ToJSON LogLoc where
    toJSON (LogLoc (Loc file _ _ start end)) =
        JSON.object ["filename" .= file, "start" .= start, "end" .= end]
    toEncoding (LogLoc (Loc file _ _ start end)) =
        JSON.pairs ("filename" .= file <> "start" .= start <> "end" .= end)

-- | Lift a location into an Exp.
liftLoc :: Loc -> Q Exp
liftLoc (Loc a b c (d1, d2) (e1, e2)) = [|(LogLoc . Loc)
    $(TH.lift a)
    $(TH.lift b)
    $(TH.lift c)
    ($(TH.lift d1), $(TH.lift d2))
    ($(TH.lift e1), $(TH.lift e2))
    |]

-- | Get current 'LogLoc'.
logLoc :: Q Exp
logLoc = [| $(TH.location >>= liftLoc) |]

newtype LogThreadId = LogThreadId ThreadId deriving (Show, Eq, Ord)

instance TextShow LogThreadId where
    showb (LogThreadId tid) = (showb . show) tid

instance ToJSON LogThreadId where
    toJSON = toJSON . show
    toEncoding = toEncoding . show

logThreadId :: (MonadIO m) => m LogThreadId
logThreadId = fmap LogThreadId (liftIO myThreadId)

-----------------------------------------------------------------------------------------
