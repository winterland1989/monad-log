{-# LANGUAGE TemplateHaskell #-}

module Control.Monad.Log.LogParser where

import Data.Aeson.TH
import Control.Monad.Log
import Data.Text (Text)

-- | use this date type to parse logs produced by 'defaultJSONFormatter'
--
-- Note. you have to unline the log first.
data DefaultLog env = DefaultLog {
        level :: Level
    ,   time :: Text
    ,   env :: env
    ,   msg :: Text
    }

$(deriveJSON defaultOptions ''DefaultLog)
