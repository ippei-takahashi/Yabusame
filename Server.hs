{-# LANGUAGE OverloadedStrings #-}

module Server where

import Control.Monad
import Control.Concurrent
import Data.ByteString (ByteString)
import Network
import Network.Wai.Handler.Warp
import System.IO
import System.Exit
import System.Posix

import App
import Signal
import State

server :: Socket -> StateRef -> IO ()
server sock stt = do 
  setHandler sigTERM stopHandler
  setHandler sigHUP reloadHandler
  reload sock stt
 where
  stopHandler = Catch $ do
    exitImmediately ExitSuccess
  reloadHandler = Catch $ do
    void $ forkIO (reload sock stt)

reload :: Socket -> StateRef -> IO ()
reload sock stt = do
  runSettingsSocket setting sock defaultApplication
 where
  setting = defaultSettings 
    { settingsPort        = 8236
    , settingsOnOpen      = increment stt
    , settingsOnClose     = decrement stt
    , settingsTimeout     = 30
    , settingsHost        = HostAny
    , settingsFdCacheDuration     = 10
    , settingsResourceTPerRequest = False }
