{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Control.Monad.Log.Label where

import Control.Monad.Log
import qualified Data.Aeson as JSON
import Data.Aeson (ToJSON, toJSON, Value(Null), toEncoding, fromEncoding)
import Data.Text (Text)

data Label = Label Text

instance TextShow Label where
    showb (Label t) = showb t

instance ToJSON Label where
    toJSON (Label t) = toJSON t
    toEncoding (Label t) = toEncoding t

withLabel :: (MonadLog Label m) => Label -> m a -> m a
withLabel = withEnv
