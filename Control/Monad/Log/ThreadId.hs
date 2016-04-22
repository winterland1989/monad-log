{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Control.Monad.Log.ThreadId where

import Control.Monad.Log
import Control.Monad.IO.Class
import qualified Data.Aeson as JSON
import Data.Aeson (ToJSON, toJSON, Value(Null), toEncoding, fromEncoding)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Control.Concurrent as Conc

newtype ThreadId = ThreadId Text deriving (Show, Eq, Ord)

instance TextShow ThreadId where
    showb (ThreadId t) = showb t

instance ToJSON ThreadId where
    toJSON (ThreadId t) = toJSON t
    toEncoding (ThreadId t) = toEncoding t

myThreadId :: (MonadLog env m) => m ThreadId
myThreadId = liftIO $ fmap (ThreadId . T.pack . show) Conc.myThreadId

withThreadId :: (MonadLog ThreadId m) => ThreadId -> m a -> m a
withThreadId = withEnv

withMyThreadId :: (MonadLog ThreadId m) => m a -> m a
withMyThreadId ma = do
    tid <-  myThreadId
    withThreadId tid ma
