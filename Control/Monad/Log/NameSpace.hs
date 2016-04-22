{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Control.Monad.Log.NameSpace where

import Control.Monad.Log
import qualified Data.Aeson as JSON
import Data.Aeson (ToJSON, toJSON, Value(Null), toEncoding, fromEncoding)
import Data.Text (Text)
import qualified Data.Text as T


newtype NameSpace = NameSpace [Text]

instance TextShow NameSpace where
    showb (NameSpace names) = T.intercalate ">>" names

instance ToJSON NameSpace where
    toJSON (NameSpace t) = toJSON t
    toEncoding (NameSpace t) = toEncoding t

withNameSpace :: (MonadLog NameSpace m) => NameSpace -> m a -> m a
withNameSpace = withEnv

subNameSpace :: (MonadLog NameSpace m) => Text -> m a -> m a
subNameSpace sub = localLogger $ \ logger ->
    logger{ environment = environment logger ++ [sub] }


newtype NameSpaceR = NameSpaceR [Text]

instance TextShow NameSpaceR where
    showb (NameSpaceR names) = T.intercalate "<<" names

instance ToJSON NameSpaceR where
    toJSON (NameSpaceR t) = toJSON t
    toEncoding (NameSpaceR t) = toEncoding t

withNameSpaceR :: (MonadLog NameSpaceR m) => NameSpaceR -> m a -> m a
withNameSpaceR = withEnv

subNameSpaceR :: (MonadLog NameSpace m) => Text -> m a -> m a
subNameSpaceR sub = localLogger $ \ logger ->
    logger{ environment = sub : environment logger }
