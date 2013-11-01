module Tables.Test where

import Table

data Test = Test
  { id :: Maybe Int
  , name :: Maybe String 
  } deriving (Eq, Show)

instance Table Test
