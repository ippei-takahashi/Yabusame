module Main where

import Control.Monad
import Network
import System.Posix.Process

import Loop
import Server
import State

main :: IO ()
main = withSocketsDo $ do
  sock <- listenOn (PortNumber 8236)
  cids <- replicateM 10 $ forkProcess $ do
    stt <- initStateRef
    server sock stt
  parentLoop
