{-# LANGUAGE GADTs #-}
module Language.APIDef.APIDef where

import Data.Text as T

type Name = T.Text

data Const
    = CI Int
    | CBI Integer
    | CB8 Int -- For now, since we don't have Bits types. We need to
                -- make sure the Integer remains in range
    | CB16 Int
    | CB32 Int
    | CB64 Integer
    | CStr T.Text
    | CCh Char
    | CDb Double
    | CWorldVal

    | CIntType
    | CIntegerType
    | CBits8Type
    | CBits16Type
    | CBits32Type
    | CBits64Type
    | CStringType
    | CCharType
    | CDoubleType
    | CWorldType
    deriving(Show, Read)


data Namespace = DMkNS [T.Text]
  deriving(Show, Read)

data ModuleIdent where
  DMkMI :: [T.Text] -> ModuleIdent
  deriving(Show, Read)

data DDecl where
  DClaim :: DTypeDecl -> DDecl
  DDef :: [DClause] -> DDecl
  DData :: T.Text -> DDataDecl -> DDecl
  DRecord :: T.Text -> Name ->  Maybe Name -> [DField] -> DDecl -- should we keep params?
  DMutual :: [DDecl] -> DDecl
  DNamespace :: Namespace -> [DDecl] -> DDecl
  DDeclNotImplemented:: T.Text -> DDecl
  deriving(Show, Read)

data DField where
  MkDField :: T.Text -> Name -> DTerm -> DField
  deriving(Show, Read)

data DTerm where
  DRef :: Name -> DTerm
  DPi :: Maybe Name -> DTerm -> DTerm -> DTerm
  DLam :: DTerm -> DTerm -> DTerm -> DTerm
  DApp :: DTerm -> DTerm -> DTerm
  DNamedApp :: DTerm -> Name -> DTerm -> DTerm  
  DPrimVal :: Const -> DTerm
  DImplicit :: DTerm    
  DInfer :: DTerm
  DHole :: T.Text -> DTerm
  DType :: DTerm
  DList :: [DTerm] -> DTerm
  DPair :: DTerm -> DTerm -> DTerm
  DUnit :: DTerm
  DBracketed ::  DTerm -> DTerm
  DTermNotImplemented :: T.Text -> DTerm
  deriving(Show, Read)

data DClause where
  MkDPatClause :: DTerm -> DTerm -> DClause
  MkDClauseNotImplemented :: T.Text -> DClause
  deriving(Show, Read)

data DTypeDecl where
  MkDTy :: Name -> T.Text -> DTerm -> DTypeDecl
  deriving(Show, Read)

data DDataDecl where
  MkDData :: Name -> DTerm -> [DTypeDecl] -> DDataDecl
--  MkDLater :: Name -> DTerm -> DDataDecl
  MkDataDeclNotImplemented :: T.Text -> DDataDecl
  deriving(Show, Read)

