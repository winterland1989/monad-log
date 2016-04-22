{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Control.Monad.Log.Loc where

import Control.Monad.Log
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Aeson as JSON
import Data.Aeson (ToJSON, toJSON, toEncoding, fromEncoding, (.=))
import Data.Monoid ((<>))

import Language.Haskell.TH.Syntax (Q, Exp)
import qualified Language.Haskell.TH.Syntax as TH

-- | a wrapper around 'Loc'
data Loc = Loc {
        filename :: Text
    ,   module'  :: Text
    ,   package  :: Text
    ,   line     :: Int
    } deriving (Show, Eq, Ord)

instance TextShow Loc where
    showb (Loc f m p l) = showb f <> showb m <> showb p <> showb l

instance ToJSON Loc where
    toJSON (Loc f m p l) =
        JSON.object ["filename" .= f, "module" .= m, "package" .= p]
    toEncoding (Loc f m p l) =
        JSON.pairs ("filename" .= f <> "module" .= m <> "package" .= p)

-- | Lift a location into an Exp.
liftLoc :: TH.Loc -> Q Exp
liftLoc (TH.Loc f m p (l, _) _) = [|Loc
    (T.pack $(TH.lift f))
    (T.pack $(TH.lift m))
    (T.pack $(TH.lift p))
    $(TH.lift l)
    |]

-- | Get current 'Loc'.
logLoc :: Q Exp
logLoc = [| $(TH.location >>= liftLoc) |]

withLoc :: (MonadLog Loc m) => Loc -> m a -> m a
withLoc = withEnv
