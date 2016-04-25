{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}

-- | This module provides a mtl style 'MonadLog' class and a concrete monad transformer 'LogT'.
--
-- If you are an application author, you can use 'LogT' transformer,
-- a specialized reader monad to inject 'Logger'.
--
-- If you are a library author, you should:
--
--     * make your monad stack an instance of 'MonadLog', usually you can do this by embedding a 'Logger' into your monad's reader part.
--
--     * provide a default formatter, and API to run with customized formatter.
--
module Control.Monad.Log (
    -- * parametrized 'Logger' type
      Level(..)
    , levelDebug
    , levelInfo
    , levelWarning
    , levelError
    , levelCritical
    , Logger(..)
    , envLens
    , makeLogger
    , makeDefaultLogger
    , makeDefaultJSONLogger
    , defaultFormatter
    , defaultJSONFormatter
    -- * 'MonadLog' class
    , MonadLog(..)
    , withFilterLevel
    , withEnv
    , localEnv
    -- * LogT, a concrete monad transformaer
    , LogT(..)
    , runLogTSafe
    , runLogTSafeBase
    , runLogT'
    -- * logging functions
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
    -- * re-export from text-show and fast-logger
    , LogStr
    , toLogStr
    , LogType(..)
    , FileLogSpec(..)
    , TimeFormat
    , FormattedTime
    , simpleTimeFormat
    , simpleTimeFormat'
    , module X
    ) where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative
import Data.Monoid (Monoid)
#endif
#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail as Fail
#endif
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
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS (RWST, mapRWST)
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS (RWST, mapRWST)
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict

import System.Log.FastLogger
import Prelude hiding (log, error)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Builder as BB
import TextShow as X

import qualified Data.Aeson as JSON
import Data.Aeson (ToJSON, fromEncoding, (.=))
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

-- | Alias for @Level 0@
levelDebug :: Level
levelDebug = Level 0

-- | Alias for @Level 1@
levelInfo :: Level
levelInfo = Level 1

-- | Alias for @Level 2@
levelWarning :: Level
levelWarning = Level 2

-- | Alias for @Level 3@
levelError :: Level
levelError = Level 3

-- | Alias for @Level 4@
levelCritical :: Level
levelCritical = Level 4

-- | A logger type parametrized by an extra environment type.
data Logger env = Logger {
        filterLevel  :: {-# UNPACK #-} !Level  -- ^ filter level, equal or above it will be logged.
    ,   environment  :: env                    -- ^ parametrized logging environment.
    ,   formatter    :: Level -> FormattedTime -> env -> Text -> LogStr -- ^ formatter function.
    ,   timeCache    :: IO FormattedTime       -- ^ a time cache to avoid cost of frequently formatting time.
    ,   logger       :: LogStr -> IO ()        -- ^ a 'FastLogger' log function.
    ,   cleanUp      :: IO ()                  -- ^ clean up action(flushing/closing file...).
    }

-- | Lens for 'environment'.
envLens :: (Functor f) => (env -> f env) -> Logger env -> f (Logger env)
envLens f (Logger fltr e fmt t l c) = fmap (\ e' -> Logger fltr e' fmt t l c) (f e)

-- | make a 'Logger' based on 'FastLogger'.
makeLogger :: (MonadIO m)
    => (Level -> FormattedTime -> env -> Text -> LogStr)  -- ^ formatter function
    -> TimeFormat                                         -- ^ check "System.Log.FastLogger.Date"
    -> LogType
    -> Level                                              -- ^ filter level
    -> env                                                -- ^ init environment
    -> m (Logger env)
makeLogger fmt tfmt typ fltr env = liftIO $ do
    tc <- newTimeCache tfmt
    (fl, cl) <- newFastLogger typ
    return $ Logger fltr env fmt tc fl cl

-- | make a 'Logger' with 'defaultFormatter'.
makeDefaultLogger :: (MonadIO m, TextShow env)
    => TimeFormat
    -> LogType
    -> Level
    -> env
    -> m (Logger env)
makeDefaultLogger = makeLogger defaultFormatter

-- | make a 'Logger' with 'defaultJSONFormatter'.
makeDefaultJSONLogger :: (MonadIO m, ToJSON env)
    => TimeFormat
    -> LogType
    -> Level
    -> env
    -> m (Logger env)
makeDefaultJSONLogger = makeLogger defaultJSONFormatter

-- | a default formatter with following format:
--
-- @[LEVEL] [TIME] [ENV] LOG MESSAGE\\n@
defaultFormatter :: (TextShow env) => Level -> FormattedTime -> env -> Text -> LogStr
defaultFormatter lvl time env msg = toLogStr . T.concat $
    [ "[" , showt lvl, "] [", T.decodeUtf8 time,  "] [",  showt env, "] " , msg , "\n" ]

-- | a default JSON formatter with following format:
--
-- @{"level": "LEVEL", "time": "TIME", "env": "ENV", "msg": "LOG MESSAGE" }\\n@
defaultJSONFormatter :: (ToJSON env) => Level -> FormattedTime -> env -> Text -> LogStr
defaultJSONFormatter lvl time env msg = toLogStr . BB.toLazyByteString $
    ( fromEncoding . JSON.pairs $
        "level" .= showt lvl
        <> "time" .=  T.decodeUtf8 time
        <> "env" .= env
        <> "msg" .= msg
    ) <> "\n"

-----------------------------------------------------------------------------------------

-- | This is the main class for using logging function in this package.
--
-- provide an instance for 'MonadLog' to log within your monad stack.
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

instance (MonadLog env m, Monoid w) => MonadLog env (LazyRWS.RWST r w s m) where
    askLogger   = lift askLogger
    localLogger = LazyRWS.mapRWST . localLogger

instance (MonadLog env m, Monoid w) => MonadLog env (StrictRWS.RWST r w s m) where
    askLogger   = lift askLogger
    localLogger = StrictRWS.mapRWST . localLogger

-- | run 'MonadLog' within a new 'FilterLevel'.
withFilterLevel :: (MonadLog env m) => Level -> m a -> m a
withFilterLevel level = localLogger (\ lgr -> lgr{ filterLevel = level})

-- | run 'MonadLog' within a new environment.
withEnv :: (MonadLog env m) => env -> m a -> m a
withEnv env = localLogger (\ lgr -> lgr{ environment = env })

-- | run 'MonadLog' within a modified environment.
localEnv :: (MonadLog env m) => (env -> env) -> m a -> m a
localEnv f = localLogger $ \ lgr -> lgr { environment = f (environment lgr) }

-----------------------------------------------------------------------------------------

-- | A simple 'MonadLog' instance.
--
-- a special reader monad which embed a 'Logger'.
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
    LogT ma >>= f = LogT $ \lgr -> do
        a <- ma lgr
        let LogT f' = f a
        f' lgr
    {-# INLINE (>>=) #-}
    fail msg = lift (fail msg)
    {-# INLINE fail #-}

#if MIN_VERSION_base(4,9,0)
instance Fail.MonadFail m => Fail.MonadFail (LogT env m) where
    fail msg = lift (Fail.fail msg)
    {-# INLINE fail #-}
#endif

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

-- | safely run 'LogT' inside 'MonadMask'. Logs are guaranteed to be flushed on exceptions.
runLogTSafe :: (MonadIO m, MonadMask m) => Logger env -> LogT env m a -> m a
runLogTSafe lgr m = finally (runLogT m lgr) (liftIO $ cleanUp lgr)

-- | safely run 'LogT' inside 'MonadBaseControl IO m'. Logs are guaranteed to be flushed on exceptions.
runLogTSafeBase :: (MonadBaseControl IO m, MonadIO m) => Logger env -> LogT env m a -> m a
runLogTSafeBase lgr m = Lifted.finally (runLogT m lgr) (liftIO $ cleanUp lgr)

-- | @runLogT' = flip runLogT@, run 'LogT' without clean up.
-- usually used inside different threads so that an exception won't clean up 'Logger'.
runLogT' :: (MonadIO m) => Logger env -> LogT env m a -> m a
runLogT' = flip runLogT

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
debug = log levelDebug

info :: (MonadLog env m) => Text -> m ()
info = log levelInfo

warning :: (MonadLog env m) => Text -> m ()
warning = log levelWarning

error :: (MonadLog env m) => Text -> m ()
error = log levelError

critical :: (MonadLog env m) => Text -> m ()
critical = log levelCritical

debug' :: (MonadLog env m) => env -> Text -> m ()
debug' = log' levelDebug

info' :: (MonadLog env m) => env -> Text -> m ()
info' = log' levelInfo

warning' :: (MonadLog env m) => env -> Text -> m ()
warning' = log' levelWarning

error' :: (MonadLog env m) => env -> Text -> m ()
error' = log' levelError

critical' :: (MonadLog env m) => env -> Text -> m ()
critical' = log' levelCritical
