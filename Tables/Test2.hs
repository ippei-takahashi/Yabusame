module Tables.Test2 where

import Table

data Test2 = Test2
  { id :: Int
  , value :: String } deriving (Eq, Show)

instance Table Test2
