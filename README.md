A fast & simple logging monad
=============================

[![Hackage](https://img.shields.io/hackage/v/monad-log.svg?style=flat-square)](http://hackage.haskell.org/package/monad-log)

This package provide a mtl style `MonadLog` class and a concrete monad transformer `LogT`, the main difference between this package and monad-logger are:

+ Base monad has to be an instance of `MonadIO`.

+ Parametrized logging environment for extensibility.

+ Basic logging environment type(`Label`,`Loc`,`NameSpace`,`ThreadId`) are included, and you can easily make your own.

+ JSON logging built-in.

+ default to fast-logger backend, with good stdout and file support.

If you are an application author, you can use `LogT` transformer, it's just a specialized reader monad to inject `Logger env`.

If you are a library author, you should do following two things:

+ make your monad stack an instance of 'MonadLog', usually you can do this by embedding `Logger env` into your monad's reader part.

+ provide a default formatter, and API to run with customized formatter.

Example
-------

+ A simple labelled logger:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Log
import Control.Monad.Log.Label

-- Following log will be output to stdout:
-- [INFO] [25-Apr-2016 12:51:56] [main] This is simple log 1
-- [INFO] [25-Apr-2016 12:51:56] [foo] This is simple log 2

main :: IO ()
main = do
    logger <- makeDefaultLogger
        simpleTimeFormat'
        (LogStdout 4096)
        levelDebug
        (Label "main")

    runLogTSafe logger $ do
        info "This is simple log 1"

        withLabel (Label "foo") $ do
            info "This is simple log 2"

```

+ Logging with ThreadId:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Log
import Control.Monad.IO.Class
import Control.Monad
import Control.Concurrent
import Control.Monad.Log.LogThreadId

-- Following log will be output to stdout:
-- [INFO] [25-Apr-2016 15:06:10] [ThreadId 671] This is simple log 1
-- [INFO] [25-Apr-2016 15:06:10] [ThreadId 674] This is simple log 2
-- [INFO] [25-Apr-2016 15:06:10] [ThreadId 675] This is simple log 2
-- [INFO] [25-Apr-2016 15:06:10] [ThreadId 676] This is simple log 2
-- [INFO] [25-Apr-2016 15:06:10] [ThreadId 677] This is simple log 2
-- [INFO] [25-Apr-2016 15:06:10] [ThreadId 678] This is simple log 2
-- [INFO] [25-Apr-2016 15:06:10] [ThreadId 679] This is simple log 2
...

main :: IO ()
main = do
    tid <- myLogThreadId
    logger <- makeDefaultLogger
        simpleTimeFormat'
        (LogStdout 4096)
        levelDebug
        tid

    runLogTSafe logger $ do
        info "This is simple log 1"

    replicateM_ 100 $
        forkIO . runLogT' logger . withMyLogThreadId $ do
            info "This is simple log 2"
```

+ Customized logging environment:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.Log
import Control.Monad.Log.LogLoc
import Control.Monad.Log.NameSpace
import Data.Aeson.TH
import Data.Text (Text)

-- Following JSON log will be output to stdout:
-- {"level":"INFO","time":"25-Apr-2016 13:54:32"
-- ,"env":{"loc":{"filename":"Test.hs","module":"Test","package":"monad_GM54RwU2jZ84vGJIhnMYMH","line":33},"ns":["root"]}
-- ,"msg":"This is simple log 1"}
-- {"level":"INFO","time":"25-Apr-2016 13:54:32"
-- ,"env":{"loc":{"filename":"Test.hs","module":"Test","package":"monad_GM54RwU2jZ84vGJIhnMYMH","line":33},"ns":["foo","root"]}
-- ,"msg":"This is simple log 2"}

-- | Define your logging environment type.
-- To use 'defaultFomatter', provide a 'TextShow' instance
-- To use 'defaultJSONFomatter', provide a 'ToJSON' instance

data MyEnv = MyEnv {
        loc :: LogLoc  -- This is shared by every log within one 'MonadLog'.
    ,   ns  :: NameSpace
    } deriving (Show, Eq)

$(deriveJSON defaultOptions ''MyEnv)

subMyNS :: (MonadLog MyEnv m) => Text -> m a -> m a
subMyNS sub = localEnv $ \env -> env { ns = pushNameSpace sub (ns env) }

main :: IO ()
main = do
    logger <- makeDefaultJSONLogger
        simpleTimeFormat'
        (LogStdout 4096)
        levelDebug
        (MyEnv $myLogLoc (NameSpace ["root"]))

    runLogTSafe logger $ do
        info "This is simple log 1"

        subMyNS "foo" $ do
            info "This is simple log 2"

```
