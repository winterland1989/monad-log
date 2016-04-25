{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Control.Monad.Log.Label where

import Control.Monad.Log
import Data.Aeson
import Data.Text (Text)

-- | Simple 'Label' environment for labelled logging.
--
-- @
-- showt (Label "foo") = "foo"
-- toJSON (Label "foo") = "foo"
-- @
data Label = Label Text deriving (Show, Eq, Ord)

instance TextShow Label where
    showb (Label t) = fromText t

instance ToJSON Label where
    toJSON (Label t) = toJSON t
    toEncoding (Label t) = toEncoding t

instance FromJSON Label where
    parseJSON t = Label <$> parseJSON t

-- | 'withEnv' specialized for 'Label'
withLabel :: (MonadLog Label m) => Label -> m a -> m a
withLabel = withEnv
