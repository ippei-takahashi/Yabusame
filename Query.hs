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

strSelect :: From -> Q String
strSelect frm = do
  v <- validate frm
  return $ if v 
    then
      "SELECT * FROM " ++
      show frm
    else 
      undefined

  
