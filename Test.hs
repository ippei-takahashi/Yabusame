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
  q <- strQuery [| Select [column T.id] |] [''T.Test, ''T2.Test2]
  runIO $ quickQuery con q [] >>= print
  return []
