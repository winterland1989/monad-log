{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Control.Monad.Log.LogThreadId where

import Control.Monad.Log
#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative
#endif
import Control.Monad.IO.Class
import Control.Concurrent
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T

-- | a formatted 'LogThreadId'.
--
-- @
-- showt (LogThreadId "LogThreadId x") = "LogThreadId x"
-- toJSON (LogThreadId "LogThreadId x") = "LogThreadId x"
-- @
newtype LogThreadId = LogThreadId Text deriving (Show, Eq, Ord)

instance TextShow LogThreadId where
    showb (LogThreadId t) = fromText t

instance ToJSON LogThreadId where
    toJSON (LogThreadId t) = toJSON t
#if MIN_VERSION_aeson(0,10,0)
    toEncoding (LogThreadId t) = toEncoding t
#endif

instance FromJSON LogThreadId where
    parseJSON t = LogThreadId <$> parseJSON t

-- | Get current 'LogThreadId'.
myLogThreadId :: (MonadIO m) => m LogThreadId
myLogThreadId = liftIO $ fmap (LogThreadId . T.pack . show) myThreadId

-- | 'withEnv' specialized for 'LogThreadId'
withLogThreadId :: (MonadLog LogThreadId m) => LogThreadId -> m a -> m a
withLogThreadId = withEnv

-- | obtain 'LogThreadId' and change logging environment.
withMyLogThreadId :: (MonadLog LogThreadId m) => m a -> m a
withMyLogThreadId ma = do
    tid <-  myLogThreadId
    withLogThreadId tid ma
