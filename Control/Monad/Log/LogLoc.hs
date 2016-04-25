{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Control.Monad.Log.LogLoc where

import Control.Monad.Log
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Internal.Builder (fromText)
import qualified Control.Concurrent as Conc
import Data.Aeson
import Data.Monoid ((<>))

import Language.Haskell.TH.Syntax (Q, Exp)
import qualified Language.Haskell.TH.Syntax as TH

-- | source location information.
--
-- @
-- showt (LogLoc "package" "Module" "file.hs" 122) = "package Module file.hs 122"
-- toJSON (LogLoc "package" "Module" "file.hs" 122) =
--     '{"package":"package","module":"module","filename":"file.hs","line":122}'
-- @
data LogLoc = LogLoc {
        package  :: Text
    ,   module'  :: Text
    ,   filename :: Text
    ,   line     :: Int
    } deriving (Show, Eq, Ord)

instance TextShow LogLoc where
    showb (LogLoc p m f l) = fromText (T.intercalate " " [p, m, f, showt l])

instance ToJSON LogLoc where
    toJSON (LogLoc p m f l) =
        object ["filename" .= f, "module" .= m, "package" .= p, "line" .= l]
    toEncoding (LogLoc p m f l) =
        pairs ("filename" .= f <> "module" .= m <> "package" .= p <> "line" .= l)

instance FromJSON LogLoc where
    parseJSON (Object v) = LogLoc <$>
        v .: "package" <*>
        v .: "module" <*>
        v .: "filename" <*>
        v .: "line"

-- | Lift a location into an Exp.
liftLogLoc :: TH.Loc -> Q Exp
liftLogLoc (TH.Loc f p m (l, _) _) = [|LogLoc
    (T.pack $(TH.lift p))
    (T.pack $(TH.lift m))
    (T.pack $(TH.lift f))
    $(TH.lift l)
    |]

-- | Get current 'LogLoc'.
myLogLoc :: Q Exp
myLogLoc = [| $(TH.location >>= liftLogLoc) |]

-- | 'withEnv' specialized for 'LogLoc'
withLogLoc :: (MonadLog LogLoc m) => LogLoc -> m a -> m a
withLogLoc = withEnv
