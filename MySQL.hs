module MySQL where

import Database.HDBC
import Database.HDBC.MySQL
import Language.Haskell.TH

import Query

dbDriver = "mysql"
dbServer = "127.0.0.1"
dbDatabase = "yabusame"
dbUserName = "root"
dbPassword = "yabusame"
  
connDB = connectMySQL defaultMySQLConnectInfo 
  { mysqlHost = dbServer
  , mysqlDatabase = dbDatabase
  , mysqlPassword = dbPassword
  , mysqlUser = dbUserName }

