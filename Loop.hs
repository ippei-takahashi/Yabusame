module Loop where

import Control.Concurrent

parentLoop :: IO ()
parentLoop = do
  threadDelay 10000000
  parentLoop
