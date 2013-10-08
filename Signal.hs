module Signal where

import Control.Monad
import System.Posix

setHandler :: Signal -> Handler -> IO ()
setHandler sig func = void $ installHandler sig func Nothing
