{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Control.Monad.Log.Label where

import Control.Monad.Log
#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative
#endif
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
#if MIN_VERSION_aeson(0,10,0)
    toEncoding (Label t) = toEncoding t
#endif

instance FromJSON Label where
    parseJSON t = Label <$> parseJSON t

-- | 'withEnv' specialized for 'Label'
withLabel :: (MonadLog Label m) => Label -> m a -> m a
withLabel = withEnv
