module Language.APIDef

import Data.List

{-
As source AST of Idris2 is too big to describe API.
To minimize efforts, reduced version of AST is defined.
Because of its reduction, if we just remove unnecessary definition, some functions becomes partial,
we choose to define NotIMplemented definition for such reduced definitions.

The idea behind this is to use restricted Idris2's source language as a API definition language.
That provides automatic type check for API definition language and an ability to use Idris2's dependent type
for API definitions.

-}


public export
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

  


export
isType : Const -> Bool
isType (CI x) = False
isType (CBI x) = False
isType (CB8 x) = False
isType (CB16 x) = False
isType (CB32 x) = False
isType (CB64 x) = False
isType (CStr x) = False
isType (CCh x) = False
isType (CDb x) = False
isType CWorldVal = True
isType CIntType = True
isType CIntegerType = True
isType CBits8Type = True
isType CBits16Type = True
isType CBits32Type = True
isType CBits64Type = True
isType CStringType = True
isType CCharType = True
isType CDoubleType = True
isType CWorldType = True


quote: String -> String
quote s = "("++ s ++ ")"

export
sconcat : String -> List String -> String
sconcat sep xs = concat $ intersperse sep xs

-- Restricted version of Source syntax

mutual
  public export
  Name : Type
  Name = String
  
  ||| DDecl is a reduced version of PDecl.
  ||| Anything ommited becomes DDeclNotIMplemented.
  public export
  data DDecl : Type where
    DClaim : DTypeDecl -> DDecl
    DDef : List DClause -> DDecl
    DData : (doc : String) -> DDataDecl -> DDecl
    DRecord : (doc : String) -> APIDef.Name -> (params : List APIDef.Name) -> (conName : Maybe APIDef.Name) -> List DField -> DDecl
    DMutual : List DDecl -> DDecl
    DDeclNotImplemented: String -> DDecl

  ||| DField is a reduced version of PDField.
  public export  
  data DField : Type where
    MkDField : (doc : String) -> APIDef.Name -> (ty : DTerm) -> DField

  ||| DTerm is a reduced version of PTerm.
  ||| This is one of the most central pert of APIDef.
  ||| Selected terms are all necessary to define data type, record, muti-argument function definitions and etc.
  public export  
  data DTerm: Type where
    DRef : APIDef.Name -> DTerm
    DPi : Maybe APIDef.Name -> (argTy : DTerm) -> (retTy : DTerm) -> DTerm
    DLam : DTerm -> (argTy : DTerm) -> (scope: DTerm) -> DTerm
    DApp : DTerm -> DTerm -> DTerm
    DPrimVal : Const -> DTerm
    DImplicit : DTerm    
    DInfer : DTerm
    DHole : String -> DTerm
    DType : DTerm
    DUnit : DTerm
    DBracketed :  DTerm -> DTerm
    DTermNotSupported : String -> DTerm
    
  ||| DClause is a reduced version of PClause.
  public export  
  data DClause : Type where
    MkDPatClause : (lhs : DTerm) -> (rhs : DTerm) -> DClause
    MkDClauseNotSupported : String -> DClause


  ||| Reduced vaersion of DTypeDecl
  public export   
  data DTypeDecl : Type where
    MkDTy : (n : APIDef.Name) -> (doc : String) -> (type : DTerm) -> DTypeDecl

  ||| Reduced vaersion of DDataDecl
  public export
  data DDataDecl : Type where
    MkDData : (tyname : APIDef.Name) -> (tycon : DTerm) -> (datacons : List DTypeDecl) -> DDataDecl
    MkDataDeclNotSUpported : String -> DDataDecl



  export
  Show DTerm where
    show (DRef x) = x
    show (DPi Nothing argTy retTy) = show argTy ++ "->" ++ show retTy
    show (DPi (Just x) argTy retTy) = "Π" ++ x ++ ":" ++ show argTy ++ "->" ++ show retTy
    show (DLam x argTy scope) = quote("λ" ++ show x ++ "." ++ show argTy ++ "-->" ++ show scope)
    show (DApp x y) = quote(show x) ++ " " ++  show y
    show (DPrimVal (CI x)) = show x
    show (DPrimVal (CBI x)) = show x
    show (DPrimVal (CB8 x)) = show x
    show (DPrimVal (CB16 x)) = show x
    show (DPrimVal (CB32 x)) = show x
    show (DPrimVal (CB64 x)) = show x
    show (DPrimVal (CStr x)) = show x
    show (DPrimVal (CCh x)) = show x
    show (DPrimVal (CDb x)) = show x
    show (DPrimVal CWorldVal) = "World"
    show (DPrimVal CIntType) = "Int"
    show (DPrimVal CIntegerType) = "Integer"
    show (DPrimVal CBits8Type) = "B8"
    show (DPrimVal CBits16Type) = "B16"
    show (DPrimVal CBits32Type) = "B32"
    show (DPrimVal CBits64Type) = "B64"
    show (DPrimVal CStringType) = "String"
    show (DPrimVal CCharType) = "Char"
    show (DPrimVal CDoubleType) = "Double"
    show (DPrimVal CWorldType) = "#World"
    show DImplicit = "IMPLICIT"    
    show DInfer = "INFER"
    show (DHole x) = "?" ++ x
    show DType = "Type"
    show DUnit = "()"
    show (DBracketed x) = show x
    show (DTermNotSupported msg) = "Not supported Term:" ++ msg

  export
  Show DField where
    show (MkDField d x ty) = x ++ ":" ++ (show ty)

  export 
  Show DClause where
    show (MkDPatClause lhs rhs) = show lhs ++ ":=" ++ show rhs
    show (MkDClauseNotSupported x) = "Not supported"

  export
  Show DTypeDecl where
    show (MkDTy doc n type) = n ++ ":" ++ show type

  export
  Show DDataDecl where
    show (MkDData tyname tycon datacons) = tyname ++ ":" ++ show tycon ++ " " ++ (sconcat " " $ map show datacons)
    show (MkDataDeclNotSUpported x) = "Not supported"

  export
  Show DDecl where
    show (DClaim x) = show x
    show (DDef xs) = sconcat " " $ map show xs
    show (DData doc x) = show x
    show (DRecord doc x params conName xs) 
      = "record "  ++ x ++ show conName ++ (sconcat " " $ map show params) ++ (sconcat " " $ map show xs)

    show (DMutual xs) = sconcat " " $ map show xs
    show (DDeclNotImplemented msg) = "Not implemented:" ++ msg
