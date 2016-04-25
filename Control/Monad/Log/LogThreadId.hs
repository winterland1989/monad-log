{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Control.Monad.Log.LogThreadId where

import Control.Monad.Log
import Control.Applicative
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
    toEncoding (LogThreadId t) = toEncoding t

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
