{-# Language FlexibleContexts #-}

module Table where

import Control.Applicative
import Data.Convertible
import qualified Data.HashMap.Strict as M
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

data Table = Table
  { name :: Name
  , columns :: [Name] }

type TableMap = M.HashMap (Maybe String) Table

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

validate :: From -> Q (Either TableMap String)
validate (From n) = do
  c <- getColumns n
  return $ Left $ M.singleton (nameModule n) $ Table n c
validate (f `XJoin` n) = 
  validate_ f n id
validate (f `LJoin` n `On` n1 `Equal` n2) =
  validateCond n1 n2 $ validate_ f n id 
validate (f `RJoin` n `On` n1 `Equal` n2) = 
  validateCond n1 n2 $ validate_ f n id 
validate (f `IJoin` n `On` n1 `Equal` n2) = 
  validateCond n1 n2 $ validate_ f n id 

validate_ :: From
          -> Name
          -> (String -> String) 
          -> Q (Either TableMap String)
validate_ frm n f = do
  v <- validate frm
  c <- getColumns n
  return $ emap (M.insert (nameModule n) (Table n c)) f v
 where
  emap f g = either (Left . f) (Right . g)

validateCond :: Name
             -> Name
             -> Q (Either TableMap String)
             -> Q (Either TableMap String)
validateCond n1 n2 e = do
  e1 <- e
  return $ validateCond' n2 $ validateCond' n1 e1
 where
  validateCond' n (Left t) =  
    case M.lookup (nameModule n) t of
      Just _  -> Left t
      Nothing -> Right (nameBase n ++ " is undefined column")
  validateCond' n e = e

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

