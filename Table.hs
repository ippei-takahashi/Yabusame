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
data From
  = From Name
  | From `XJoin` Name
  | From `LJoin` On
  | From `RJoin` On
  | From `IJoin` On
  
instance Show From where
  show (From n)            = nameBase n
  show (f `XJoin` n)        = 
    show f ++ " CROSS JOIN " ++ nameBase n
  show (f `LJoin` o) =  
    show f ++ " LEFT JOIN " ++ onStr (getName f) o
  show (f `RJoin` o) = 
    show f ++ " RIGHT JOIN " ++ onStr (getName f) o
  show (f `IJoin` o) = 
    show f ++ " INNER JOIN " ++ onStr (getName f) o

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

validate :: From -> Q Bool 
validate (From _)     = return True
validate (_ `XJoin` _) = return True
validate (f `LJoin` n `On` n1 `Equal` n2) = 
  validate' f n n1 n2
validate (f `RJoin` n `On` n1 `Equal` n2) = 
  validate' f n n1 n2
validate (f `IJoin` n `On` n1 `Equal` n2) = 
  validate' f n n1 n2

validate' :: From -> Name -> Name -> Name -> Q Bool
validate' f n n1 n2 = do
  v <- validate f
  c1 <- getColumns $ getName f
  c2 <- getColumns n
  return $ v && elem n1 c1 && elem n2 c2

getName :: From -> Name
getName (From n)             = n
getName (_ `XJoin` n)        = n
getName (_ `LJoin` n `On` _) = n
getName (_ `RJoin` n `On` _) = n
getName (_ `IJoin` n `On` _) = n

getColumns :: Name -> Q [Name]
getColumns name = do
  getColumns' <$> reify name
 where 
  getColumns' (TyConI (DataD _ _ _ [RecC _ xs] _)) = 
    map (\(x, _, _) -> x) xs

columnName :: Name -> Name -> String
columnName n1 n2 = nameBase n1 ++ '.' : nameBase n2

