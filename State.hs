{-# LANGUAGE BangPatterns #-}

module State where

import Control.Applicative
import Data.IORef

data State = State 
  { connectionCount :: !Int }
  
initState :: State
initState = State 0

initStateRef :: IO StateRef
initStateRef = newIORef initState

type StateRef = IORef State

getConnectionCounter :: StateRef -> IO Int
getConnectionCounter sref = connectionCount <$> readIORef sref

increment :: StateRef -> IO ()
increment sref =
  strictAtomicModifyIORef sref $ \st -> st 
    { connectionCount = connectionCount st + 1 }

decrement :: StateRef -> IO ()
decrement sref =
  strictAtomicModifyIORef sref $ \st -> st 
    { connectionCount = connectionCount st - 1 }

strictAtomicModifyIORef :: IORef a -> (a -> a) -> IO ()
strictAtomicModifyIORef ref f = do
  !_ <- atomicModifyIORef ref (\x -> let !r = f x in (r, ()))
  return ()
