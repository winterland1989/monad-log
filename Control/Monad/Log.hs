{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module Control.Monad.Log (
    -- * parametrized 'Logger' type
      Level(..)
    , Logger(..)
    , makeLogger
    , makeDefaultLogger
    , makeDefaultJSONLogger
    -- * re-export from fast-logger
    , module X
    , LogStr(..)
    , toLogStr
    , LogType(..)
    -- * 'MonadLog' class
    , MonadLog(..)
    , withFilterLevel
    , withEnv
    -- * LogT, a concrete monad transformaer
    , LogT(..)
    , runLogTSafe
    , runLogTSafe'
    -- * re-export from text-show
    , module TextShow
    -- logging functions
    , debug
    , info
    , warning
    , error
    , critical
    , debug'
    , info'
    , warning'
    , error'
    , critical'
    ) where

import Control.Monad (when, liftM, ap)
import Control.Monad.Catch (MonadMask, finally)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Control.Exception.Lifted as Lifted

import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Cont as Cont
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS (RWST, ask, local, reader)
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS (RWST, ask, local, reader)
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict

import System.Log.FastLogger
import System.Log.FastLogger.Date as X
import System.Log.FastLogger.File as X
import Prelude hiding (log, error)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import TextShow (TextShow, showt, showb)

import qualified Data.Aeson as JSON
import Data.Aeson (ToJSON, toJSON, toEncoding, fromEncoding, (.=))
import Data.Monoid ((<>))

-----------------------------------------------------------------------------------------

-- | a newtype wrapper arounded 'Int' for GHC unpacking, with following 'TextShow' instance:
--
-- > showb (Level 0) = "DEBUG"
-- > showb (Level 1) = "INFO"
-- > showb (Level 2) = "WARNING"
-- > showb (Level 3) = "ERROR"
-- > showb (Level 4) = "CRITICAL"
-- > showb (Level x) = "OTHER:" <> showb x
newtype Level = Level Int deriving (Eq, Ord, Bounded, Show, Read)

instance TextShow Level where
    showb (Level 0) = "DEBUG"
    showb (Level 1) = "INFO"
    showb (Level 2) = "WARNING"
    showb (Level 3) = "ERROR"
    showb (Level 4) = "CRITICAL"
    showb (Level x) = "OTHER:" <> showb x
    {-# inline showb #-}

lDEBUG :: Level
lDEBUG = Level 0

lINFO :: Level
lINFO = Level 1

lWARNING :: Level
lWARNING = Level 2

lERROR :: Level
lERROR = Level 3

lCRITICAL :: Level
lCRITICAL = Level 4

-- | A logger type parametrized by an extra environment type
--
-- the 'logger' type is based on 'FastLogger'
data Logger env = Logger {
        filterLevel  :: {-# UNPACK #-} !Level  -- ^ filter level, equal or above it will be logged.
    ,   environment  :: env                    -- ^ parametrized logging environment.
    ,   formatter    :: Level -> FormattedTime -> env -> Text -> LogStr -- ^ formatter function.
    ,   timeCache    :: IO FormattedTime       -- a time cache to avoid cost of frequently formatting time.
    ,   logger       :: LogStr -> IO ()        -- a 'FastLogger' log function.
    ,   cleanUp      :: IO ()                  -- clean up action(flushing/closing file...).
    }

-- | make a 'Logger' based on 'FastLogger'
makeLogger :: (MonadIO m)
    => (Level -> FormattedTime -> env -> Text -> LogStr)
    -> TimeFormat
    -> LogType
    -> Level
    -> env
    -> m (Logger env)
makeLogger fmt tfmt typ fltr env = liftIO $ do
    tc <- newTimeCache tfmt
    (fl, cleanUp) <- newFastLogger typ
    return $ Logger fltr env fmt tc fl cleanUp

makeDefaultLogger :: (MonadIO m, TextShow env)
    => TimeFormat
    -> LogType
    -> Level
    -> env
    -> m (Logger env)
makeDefaultLogger = makeLogger defaultFormatter

makeDefaultJSONLogger :: (MonadIO m, ToJSON env)
    => TimeFormat
    -> LogType
    -> Level
    -> env
    -> m (Logger env)
makeDefaultJSONLogger = makeLogger defaultJSONFormatter

defaultFormatter :: (TextShow env) => Level -> FormattedTime -> env -> Text -> LogStr
defaultFormatter lvl time env msg = toLogStr . T.concat $
    [ "[" , showt lvl, "] ", T.decodeUtf8 time, " ",  showt env, " " , msg , "\n" ]

defaultJSONFormatter :: (ToJSON env) => Level -> FormattedTime -> env -> Text -> LogStr
defaultJSONFormatter lvl time env msg = toLogStr . BB.toLazyByteString $
    ( fromEncoding . JSON.pairs $
        "level" .= showt lvl
        <> "time" .=  T.decodeUtf8 time
        <> "env" .= env
        <> "msg" .= msg
    ) <> "\n"

-----------------------------------------------------------------------------------------

-- | provide an instance for 'MonadLog' to log within your monad stack.
class (MonadIO m) => MonadLog env m | m -> env where
    askLogger :: m (Logger env)
    localLogger :: (Logger env -> Logger env) -> m a -> m a

instance MonadLog env m => MonadLog env (ContT r m) where
    askLogger   = lift askLogger
    localLogger = Cont.liftLocal askLogger localLogger

instance MonadLog env m => MonadLog env (ExceptT e m) where
    askLogger   = lift askLogger
    localLogger = mapExceptT . localLogger

instance MonadLog env m => MonadLog env (IdentityT m) where
    askLogger   = lift askLogger
    localLogger = mapIdentityT . localLogger

instance MonadLog env m => MonadLog env (ListT m) where
    askLogger   = lift askLogger
    localLogger = mapListT . localLogger

instance MonadLog env m => MonadLog env (MaybeT m) where
    askLogger   = lift askLogger
    localLogger = mapMaybeT . localLogger

instance MonadLog env m => MonadLog env (ReaderT r m) where
    askLogger   = lift askLogger
    localLogger = mapReaderT . localLogger

instance MonadLog env m => MonadLog env (Lazy.StateT s m) where
    askLogger   = lift askLogger
    localLogger = Lazy.mapStateT . localLogger

instance MonadLog env m => MonadLog env (Strict.StateT s m) where
    askLogger   = lift askLogger
    localLogger = Strict.mapStateT . localLogger

instance (Monoid w, MonadLog env m) => MonadLog env (Lazy.WriterT w m) where
    askLogger   = lift askLogger
    localLogger = Lazy.mapWriterT . localLogger

instance (Monoid w, MonadLog env m) => MonadLog env (Strict.WriterT w m) where
    askLogger   = lift askLogger
    localLogger = Strict.mapWriterT . localLogger

-----------------------------------------------------------------------------------------

-- | A simple 'MonadLog' instance.
--
-- a reader monad which embed a 'Logger'.
newtype LogT env m a = LogT { runLogT :: Logger env -> m a }

instance Monad m => Functor (LogT env m) where
    fmap = liftM
    {-# INLINE fmap #-}

instance Monad m => Applicative (LogT env m) where
    pure = return
    {-# INLINE pure #-}
    (<*>) = ap
    {-# INLINE (<*>) #-}

instance Monad m => Monad (LogT env m) where
    return = LogT . const . return
    {-# INLINE return #-}
    LogT ma >>= f = LogT $ \logger -> do
        a <- ma logger
        let LogT f' = f a
        f' logger
    {-# INLINE (>>=) #-}

instance (MonadFix m) => MonadFix (LogT r m) where
    mfix f = LogT $ \ r -> mfix $ \ a -> runLogT (f a) r
    {-# INLINE mfix #-}

instance MonadTrans (LogT env) where
    lift = LogT . const
    {-# INLINE lift #-}

instance MonadIO m => MonadIO (LogT env m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

instance MonadIO m => MonadLog env (LogT env m) where
    askLogger = LogT return
    {-# INLINE askLogger #-}
    localLogger f ma = LogT $ \ r -> runLogT ma (f r)
    {-# INLINE localLogger #-}

-- | run 'ReaderT' based 'MonadLog'
runLogTSafe :: (MonadIO m, MonadMask m) => Logger env -> LogT env m a -> m a
runLogTSafe logger m = finally (runLogT m logger) (liftIO $ cleanUp logger)

-- | run 'ReaderT' based 'MonadLog'
runLogTSafe' :: (MonadBaseControl IO m, MonadIO m) => Logger env -> LogT env m a -> m a
runLogTSafe' logger m = Lifted.finally (runLogT m logger) (liftIO $ cleanUp logger)

-----------------------------------------------------------------------------------------

-- | run 'MonadLog' within a new 'FilterLevel'
withFilterLevel :: (MonadLog env m) => Level -> m a -> m a
withFilterLevel level = localLogger (\ logger -> logger{ filterLevel = level})

withEnv :: (MonadLog env m) => env -> m a -> m a
withEnv env = localLogger (\ logger -> logger{ environment = env })

-----------------------------------------------------------------------------------------

log :: (MonadLog env m) => Level -> Text -> m ()
log lvl msg = do
    (Logger fltr env fmt tc wrt _) <- askLogger
    when (lvl >= fltr) $ liftIO $
        tc >>= \ t -> (wrt . toLogStr) (fmt lvl t env msg)
{-# INLINE log #-}

log' :: (MonadLog env m) => Level -> env -> Text -> m ()
log' lvl env msg = do
    (Logger fltr _ fmt tc wrt _) <- askLogger
    when (lvl >= fltr) $ liftIO $
        tc >>= \ t -> (wrt . toLogStr) (fmt lvl t env msg)
{-# INLINE log' #-}

debug :: (MonadLog env m) => Text -> m ()
debug = log lDEBUG

info :: (MonadLog env m) => Text -> m ()
info = log lINFO

warning :: (MonadLog env m) => Text -> m ()
warning = log lWARNING

error :: (MonadLog env m) => Text -> m ()
error = log lERROR

critical :: (MonadLog env m) => Text -> m ()
critical = log lCRITICAL

debug' :: (MonadLog env m) => env -> Text -> m ()
debug' = log' lDEBUG

info' :: (MonadLog env m) => env -> Text -> m ()
info' = log' lINFO

warning' :: (MonadLog env m) => env -> Text -> m ()
warning' = log' lWARNING

error' :: (MonadLog env m) => env -> Text -> m ()
error' = log' lERROR

critical' :: (MonadLog env m) => env -> Text -> m ()
critical' = log' lCRITICAL
