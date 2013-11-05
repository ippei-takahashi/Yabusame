{-# Language FlexibleContexts #-}

module Table where

import Control.Applicative
import Data.Convertible
import Database.HDBC
import Language.Haskell.TH

infixl 5 `XJoin`
infixl 5 `LJoin`
infixl 5 `RJoin`
infixl 5 `IJoin`
data Table
  = Table Name
  | Table `XJoin` Name
  | Table `LJoin` On
  | Table `RJoin` On
  | Table `IJoin` On
  
instance Show Table where
  show (Table n)            = nameBase n
  show (t `XJoin` n)        = 
    show t ++ " CROSS JOIN " ++ nameBase n
  show (t `LJoin` o) =  
    show t ++ " LEFT JOIN " ++ onStr (getName t) o
  show (t `RJoin` o) = 
    show t ++ " RIGHT JOIN " ++ onStr (getName t) o
  show (t `IJoin` o) = 
    show t ++ " INNER JOIN " ++ onStr (getName t) o

infixl 6 `On`
data On = Name `On` Condition

onStr :: Name -> On -> String
onStr n1 (n2 `On` c) = 
  nameBase n2 ++ " ON " ++ eqStr n1 n2 c

infixl 7 `Equal`
data Condition = Name `Equal` Name

eqStr :: Name -> Name -> Condition -> String
eqStr n1 n2 (cn1 `Equal` cn2) = 
  columnName n1 cn1 ++ " = " ++ columnName n2 cn2

validate :: Table -> Q Bool 
validate (Table _)     = return True
validate (_ `XJoin` _) = return True
validate (t `LJoin` n `On` n1 `Equal` n2) = 
  validate' t n n1 n2
validate (t `RJoin` n `On` n1 `Equal` n2) = 
  validate' t n n1 n2
validate (t `IJoin` n `On` n1 `Equal` n2) = 
  validate' t n n1 n2

validate' :: Table -> Name -> Name -> Name -> Q Bool
validate' t n n1 n2 = do
  t1 <- validate t
  c1 <- getColumns $ getName t
  c2 <- getColumns n
  return $ t1 && elem n1 c1 && elem n2 c2

getName :: Table -> Name
getName (Table n)            = n
getName (t `XJoin` _)        = getName t
getName (t `LJoin` _ `On` _) = getName t
getName (t `RJoin` _ `On` _) = getName t
getName (t `IJoin` _ `On` _) = getName t

getColumns :: Name -> Q [Name]
getColumns name = do
  getColumns' <$> reify name
 where 
  getColumns' (TyConI (DataD _ _ _ [RecC _ xs] _)) = 
    map (\(x, _, _) -> x) xs

columnName :: Name -> Name -> String
columnName n1 n2 = nameBase n1 ++ '.' : nameBase n2

