{-# Language FlexibleContexts #-}

module Table where

import Data.Convertible
import Database.HDBC

class Table t where 
  all :: t -> SqlValue
  all = const undefined

  count :: t -> SqlValue
  count = const undefined

data Join t1 t2 a 
  = CrossJoin t1 t2
  | LeftJoin t1 t2 (On t1 t2 a)
  | RightJoin t1 t2 (On t1 t2 a)
  | InnerJoin t1 t2 (On t1 t2 a)

instance (Table t1, Table t2) => Table (Join t1 t2 a)

data On t1 t2 a = On (t1 -> a) (t2 -> a)

column :: (Table t, Convertible c SqlValue) => (t -> c) -> t -> SqlValue
column = (.) toSql
