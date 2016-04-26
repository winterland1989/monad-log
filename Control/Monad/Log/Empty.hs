{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Control.Monad.Log.Empty where

import Control.Monad.Log
#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative
#endif
import Data.Aeson
import Data.Typeable

-- | an empty environment for simple logging.
--
-- @
-- showt Empty -> "∅"
-- toJSON Empty -> null
-- @
data Empty = Empty deriving (Show, Eq, Typeable)

instance TextShow Empty where
    showb Empty = "∅"

instance ToJSON Empty where
    toJSON Empty = Null
#if MIN_VERSION_aeson(0,10,0)
    toEncoding Empty = toEncoding Null
#endif

instance FromJSON Empty where
    parseJSON Null = return Empty
    parseJSON _ = mempty
