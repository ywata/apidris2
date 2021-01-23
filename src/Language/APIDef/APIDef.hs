{-# LANGUAGE GADTs #-}
module Language.APIDef.APIDef where

type Name = String

data Const
    = CI Int
    | CBI Integer
    | CB8 Int -- For now, since we don't have Bits types. We need to
                -- make sure the Integer remains in range
    | CB16 Int
    | CB32 Int
    | CB64 Integer
    | CStr String
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

data DDecl where
  DClaim :: DTypeDecl -> DDecl
  DDef :: [DClause] -> DDecl
  DData :: String -> DDataDecl -> DDecl
  DRecord :: String -> Name ->  [Name] -> Maybe Name -> [DField] -> DDecl
  DMutual :: [DDecl] -> DDecl
  DDeclNotImplemented:: String -> DDecl
  deriving(Show, Read)

data DField where
  MkDField :: String -> Name -> DTerm -> DField
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
  DHole :: String -> DTerm
  DType :: DTerm
  DList :: [DTerm] -> DTerm
  DPair :: DTerm -> DTerm -> DTerm
  DUnit :: DTerm
  DBracketed ::  DTerm -> DTerm
  DTermNotSupported :: String -> DTerm
  deriving(Show, Read)

data DClause where
  MkDPatClause :: DTerm -> DTerm -> DClause
  MkDClauseNotSupported :: String -> DClause
  deriving(Show, Read)

data DTypeDecl where
  MkDTy :: Name -> String -> DTerm -> DTypeDecl
  deriving(Show, Read)

data DDataDecl where
  MkDData :: Name -> DTerm -> [DTypeDecl] -> DDataDecl
  MkDLater :: Name -> DTerm -> DDataDecl
--  MkDataDeclNotSUpported :: String -> DDataDecl
  deriving(Show, Read)

