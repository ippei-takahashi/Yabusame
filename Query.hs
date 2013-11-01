{-# Language TemplateHaskell #-}

module Query where

import Control.Applicative
import Control.Monad
import Data.List
import Database.HDBC
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Syntax.Internals

import Table

data Query t = Select [t -> SqlValue]

strQuery :: Quasi m => Q Exp -> [Name] -> m String
strQuery exp tbls = liftM2 strQueryQ (runQ exp) columns
 where 
  columns = mapM (runQ . getColumnsWithData) tbls
  strQueryQ (AppE (ConE n) (ListE xs)) clms
    | n == 'Query.Select = 
    "SELECT " ++ 
    (foldr catStr "" $ map strColumn xs) ++
    " FROM " ++
    foldr catStr "" froms
   where
    strColumn (AppE (VarE n1) (VarE n2))
      | n1 == 'Table.column = columnName n2 clms
    strColumn (VarE n)
      | n == 'Table.count = "count(*)"
      | n == 'Table.all   = "*"
    catStr x "" = x
    catStr x y  = x ++ ", " ++ y
    froms = map nameBase tbls

columnName :: Name -> [(Name, [Name])] -> String
columnName n ((d, xs) : xss)
  | n `elem` xs = nameBase d ++ '.' : (nameBase n)
  | otherwise   = columnName n xss

getColumnsWithData :: Name -> Q (Name, [Name])
getColumnsWithData name = do
  clm <- getColumns name
  return (name, clm)

getColumns :: Name -> Q [Name]
getColumns name = do
  getColumns' <$> reify name
 where 
  getColumns' (TyConI (DataD _ _ _ [RecC _ xs] _)) = 
    map (\(x, _, _) -> x) xs

