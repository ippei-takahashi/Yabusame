module Expr where

import Database.HDBC
import Language.Haskell.TH

import Table

toTableExp :: [(Name, [Name])] 
          -> [Exp]
          -> Exp
toTableExp clms xs = ConE ''[]

