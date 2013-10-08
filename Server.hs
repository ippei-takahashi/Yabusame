{-# LANGUAGE OverloadedStrings #-}


module Server where

import Control.Monad
import Control.Concurrent
import Data.ByteString (ByteString)
import Network
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import System.IO
import System.Exit
import System.Posix

import Signal
import State

server :: Socket -> StateRef -> IO ()
server sock stt = do 
  setHandler sigTERM stopHandler
  reload sock stt
 where
  stopHandler = Catch $ do
    exitImmediately ExitSuccess
  reloadHandler = Catch $ do
    void $ forkIO (reload sock stt)

reload :: Socket -> StateRef -> IO ()
reload sock stt = do
  runSettingsSocket setting sock app
 where
  setting = defaultSettings 
    { settingsPort        = 8236
    , settingsOnOpen      = increment stt
    , settingsOnClose     = decrement stt
    , settingsTimeout     = 30
    , settingsHost        = HostAny
    , settingsFdCacheDuration     = 10
    , settingsResourceTPerRequest = False }

app :: Application
app req = do
    let st = preconditionFailed412
    return $ responseLBS st defaultHeader "Precondition Failed\r\n"
 where
  defaultHeader = [("Content-Type", "text/plain")]
