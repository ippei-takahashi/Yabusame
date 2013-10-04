module Main where

import Control.Monad
import Loop
import Network
import System.Posix.Process

main :: IO ()
main = withSocketsDo $ do
  sock <- listenOn (PortNumber 8236)
  cids <- replicateM 10 $ forkProcess (childLoop sock)
  parentLoop
