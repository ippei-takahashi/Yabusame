module MySQL where

import Database.HDBC
import Database.HDBC.MySQL
 
dbDriver = "mysql"
dbServer = "127.0.0.1"
dbDatabase = "counttest"
dbUserName = "user"
dbPassword = "pass"
  
connDB = connectMySQL defaultMySQLConnectInfo 
  { mysqlHost = dbServer
  , mysqlDatabase = dbDatabase
  , mysqlPassword = dbPassword
  , mysqlUser = dbUserName }
