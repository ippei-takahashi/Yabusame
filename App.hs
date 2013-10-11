{-# LANGUAGE OverloadedStrings #-}

module App where

import Data.ByteString (ByteString)
import Network.HTTP.Types
import Network.Wai

import MySQL

defaultApplication :: Application
defaultApplication req = do
  let st = preconditionFailed412
  return $ responseLBS st defaultHeader "Precondition Failed\r\n"
 where
  defaultHeader = [("Content-Type", "text/plain")]
