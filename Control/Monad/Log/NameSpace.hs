{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Control.Monad.Log.NameSpace where

import Control.Monad.Log
#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative
#endif
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T

-- | A newtype around a list of names from children to root.
--
-- This reversed order is choosen becasue '(:)' is faster.
--
-- @
-- showt (NameSpace ["subSub", "sub", "root"]) = "subSub<<sub<<root"
-- toJSON (NameSpace ["subSub", "sub", "root"]) = '["subSub", "sub", "root"]'
-- @
newtype NameSpace = NameSpace { getNameSpace :: [Text] } deriving (Show, Eq, Ord)

-- | push a 'Text' name to the front of 'NameSpace'.
pushNameSpace :: Text -> NameSpace -> NameSpace
pushNameSpace n (NameSpace ns) = NameSpace (n : ns)

instance TextShow NameSpace where
    showb (NameSpace names) = showb $ T.intercalate "<<" names

instance ToJSON NameSpace where
    toJSON (NameSpace t) = toJSON t
#if MIN_VERSION_aeson(0,10,0)
    toEncoding (NameSpace t) = toEncoding t
#endif

instance FromJSON NameSpace where
    parseJSON t = NameSpace <$> parseJSON t

-- | use a new 'NameSpace' within m.
withNameSpace :: (MonadLog NameSpace m) => NameSpace -> m a -> m a
withNameSpace = withEnv

-- | push a 'Text' name to the front of m's 'NameSpace'.
subNameSpace :: (MonadLog NameSpace m) => Text -> m a -> m a
subNameSpace sub = localEnv (pushNameSpace sub)
