{-# LANGUAGE Arrows #-}

module Server where

import Control.Arrow
import Control.Monad
import System.IO

run :: Handle -> IO ()
run = proc hdl -> do 
  eof <- hIsEOF -< hdl
  liftM $ hPutStrLn hdl -<< do 
    eof' <- eof
    if eof' 
      then return ""
      else hGetLine hdl
  returnA -< hClose hdl
