module Loop where

import Control.Concurrent
import Network
import Server

childLoop :: Socket -> IO ()
childLoop sock = do
  (hdl, _, _) <- accept sock
  forkIO (run hdl)
  childLoop sock

parentLoop :: IO ()
parentLoop = do
  threadDelay 10000000
  parentLoop
