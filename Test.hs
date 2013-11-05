{-# Language TemplateHaskell, QuasiQuotes #-}

module Test where

import Database.HDBC
import Database.HDBC.MySQL
import Language.Haskell.TH

import MySQL
import Query
import Table
import qualified Tables.Test as T
import qualified Tables.Test2 as T2

do
  con <- runIO $ connDB
  q <- strSelect $ 
    From ''T.Test 
    `LJoin` ''T2.Test2 
    `On` 'T.id `Equal` 'T2.id
  --runIO $ print q
  runIO $ quickQuery con q [] >>= print
  return []
