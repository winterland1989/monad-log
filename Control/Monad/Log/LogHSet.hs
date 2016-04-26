{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | This module provide a type level heterogeneous environment, example:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > {-# LANGUAGE DataKinds #-}
-- > {-# LANGUAGE TemplateHaskell #-}
-- >
-- > module Main where
-- >
-- > import Control.Monad.Log
-- > import Control.Monad.Log.LogLoc
-- > import Control.Monad.Log.NameSpace
-- > import Control.Monad.Log.LogHSet
-- >
-- > -- Following log will be output to stdout:
-- > -- [INFO] [26-Apr-2016 14:17:48] [package Main Test.hs 27, "root"] simple log 1
-- > -- [INFO] [26-Apr-2016 14:17:48] [package Main Test.hs 34, "root"] log with accurate logLoc
-- > -- [INFO] [26-Apr-2016 14:17:48] [package Main Test.hs 36, "sub<<root"] log with accurate logLoc and sub name space
-- >
-- > main :: IO ()
-- > main = do
-- >     lgr <- makeDefaultLogger
-- >         simpleTimeFormat'
-- >         (LogStdout 4096)
-- >         levelDebug
-- >         $ LogHSet (HSCons $myLogLoc (HSCons (NameSpace ["root"]) HSNil))
-- >
-- >     runLogTSafe lgr hLogging
-- >
-- > hLogging :: LogT (LogHSet '[LogLoc, NameSpace]) IO ()
-- > hLogging = do
-- >     info "simple log 1"
-- >     infoH $myLogLoc "log with accurate logLoc"
-- >     localEnvH (pushNameSpace "sub") $ do
-- >         infoH $myLogLoc "log with accurate logLoc and sub name space"

module Control.Monad.Log.LogHSet (
      LogHSet(..)
    , withEnvH
    , localEnvH
    , debugH
    , infoH
    , warningH
    , errorH
    , criticalH
    , module Data.HSet
    ) where

import Control.Monad.Log
import Control.Monad (when)
import Control.Monad.IO.Class
#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative
#endif
import qualified Data.Text as T
import Data.Text (Text)
import Data.HSet
import TypeFun.Data.List
import Data.Monoid ((<>))
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Typeable
#if !(MIN_VERSION_base(4,7,0))
import Data.Typeable.Compat
#endif

-- | an heterogeneous environment for statically typed customized logging.
--
-- @
-- showt (LogHSet (HSCons a (HSCons b HSNil))) -> "a, b"
-- toJSON (LogHSet HSNil) -> {}
-- toJSON (LogHSet (HSCons (3 :: Int) (HSCons ("ad" :: Text) HSNil))) -> {\"Int\":3, \"Text\": "ad"}
-- @
newtype LogHSet a = LogHSet { getHSet :: HSet a }

instance TextShow (LogHSet '[]) where
    showb (LogHSet HSNil) = ""

instance (TextShow e, TextShow (LogHSet els)) => TextShow (LogHSet (e ': els)) where
    showb (LogHSet (HSCons x HSNil)) = showb x
    showb (LogHSet (HSCons x xs)) = showb x <> ", " <> showb (LogHSet xs)

instance ToJSON (LogHSet '[]) where
    toJSON (LogHSet HSNil) = Object HM.empty

typeToText :: (Typeable a) => a -> Text
typeToText a = (T.pack . show . typeOf) a

repToText :: (Typeable a) => proxy a -> Text
repToText proxy = (T.pack . show . typeRep) proxy

instance (Typeable e, ToJSON e, ToJSON (LogHSet els)) => ToJSON (LogHSet (e ': els)) where
    toJSON (LogHSet (HSCons x HSNil)) = Object (HM.singleton (typeToText x) (toJSON x))
    toJSON (LogHSet (HSCons x xs)) = Object (HM.insert (typeToText x) (toJSON x) (toHashMap xs))
      where
        toHashMap xs' = let Object m = toJSON (LogHSet xs') in m

instance FromJSON (LogHSet '[]) where
    parseJSON (Object _) =  return (LogHSet HSNil)
    parseJSON _ = fail "LogHSet expect an object"

instance (NotElem e els, Typeable e, FromJSON e, FromJSON (LogHSet els)) => FromJSON (LogHSet (e ': els)) where
    parseJSON obj@(Object v) =
        case HM.lookup (repToText (Proxy :: Proxy e)) v of
            Nothing -> fail $ (show . typeRep $ (Proxy :: Proxy e)) ++ " doesn't exist"
            Just x' -> fmap LogHSet $ HSCons <$> parseJSON x' <*> (getHSet <$> parseJSON obj)
    parseJSON _ = fail "LogHSet expect an object"

logH :: (HMonoModifiable els e, MonadLog (LogHSet els) m) => Level -> e -> Text -> m ()
logH lvl e msg = do
    (Logger fltr (LogHSet env) fmt tc wrt _) <- askLogger
    when (lvl >= fltr) $ liftIO $
        tc >>= \ t -> (wrt . toLogStr) (fmt lvl t (LogHSet $ hMonoModify (const e) env) msg)
{-# INLINE logH #-}

debugH :: (HMonoModifiable els e, MonadLog (LogHSet els) m) => e -> Text -> m ()
debugH = logH levelDebug

infoH :: (HMonoModifiable els e, MonadLog (LogHSet els) m) => e -> Text -> m ()
infoH = logH levelInfo

warningH :: (HMonoModifiable els e, MonadLog (LogHSet els) m) => e -> Text -> m ()
warningH = logH levelWarning

errorH :: (HMonoModifiable els e, MonadLog (LogHSet els) m) => e -> Text -> m ()
errorH = logH levelError

criticalH :: (HMonoModifiable els e, MonadLog (LogHSet els) m) => e -> Text -> m ()
criticalH = logH levelCritical

withEnvH :: (HMonoModifiable els e, MonadLog (LogHSet els) m) =>  e -> m a -> m a
withEnvH e = localEnvH (const e)

localEnvH :: (HMonoModifiable els e, MonadLog (LogHSet els) m) => (e -> e) -> m a -> m a
localEnvH f = localEnv (\ (LogHSet env) -> LogHSet $ hMonoModify f env)

