{-# LANGUAGE GADTs #-}
module Language.APIDef (
  Name,
  Const(..),
  DDecl(..),
  DField(..),
  DTerm(..),
  DClause(..),
  DTypeDecl(..),
  DDataDecl(..),
  apiInOut)
where

import Language.APIDef.APIDef
import Language.APIDef.Utils
import Language.APIDef.PrettyPrint



