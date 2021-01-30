module Language.APIDef

import Data.Maybe
import Data.List
import Data.Strings
import Data.Vect


import Text.PrettyPrint.Prettyprinter
import Text.PrettyPrint.Prettyprinter.Util


{-
As source AST of Idris2 is too big to describe API.
To minimize efforts, reduced version of AST is defined.
Because of its reduction, if we just remove unnecessary definition, some functions becomes partial,
we choose to define NotImplemented definition for such reduced definitions.

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

pshow : Show a => a -> Doc ann
pshow = pretty . show

Pretty Const where
  pretty (CI x) = "CT" <++> pshow x
  pretty (CBI x) = "CBI" <++> pshow x
  pretty (CB8 x) = "CB8" <++> pshow x
  pretty (CB16 x) = "CB16" <++> pshow x
  pretty (CB32 x) = "CB32" <++> pshow x
  pretty (CB64 x) = "CB64" <++> pshow x
  pretty (CStr x) = "CStr" <++> pshow x
  pretty (CCh x) = "CCh" <++> pshow x
  pretty (CDb x) = "CDb" <++> pshow x
  pretty CWorldVal = "WorldVal"
  pretty CIntType = "CIntType"
  pretty CIntegerType = "CIntegerType"
  pretty CBits8Type = "CBitsType"
  pretty CBits16Type = "CBits16Type"
  pretty CBits32Type ="CBits32Type"
  pretty CBits64Type = "CBits64Type"
  pretty CStringType = "CStringType"
  pretty CCharType = "CCharType"
  pretty CDoubleType = "CDoubleType"
  pretty CWorldType = "CWorldType"



parens: String -> String
parens s = "("++ s ++ ")"

export
sconcat : String -> List String -> String
sconcat sep xs = concat $ intersperse sep xs

-- Restricted version of Source syntax
public export
data Namespace : Type where
  DMkNS : List String -> Namespace

public export
data ModuleIdent : Type where
  DMkMI : List String -> ModuleIdent


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
    DRecord : (doc : String) -> Name -> (conName : Maybe Name) -> List DField -> DDecl
    DNamespace : Namespace -> (List DDecl) -> DDecl
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
    DNamedApp : DTerm -> Name -> DTerm -> DTerm    
    DPrimVal : Const -> DTerm
    DImplicit : DTerm    
    DInfer : DTerm
    DHole : String -> DTerm
    DType : DTerm

--    DList : List DTerm -> DTerm
--    DPair : DTerm -> DTerm -> DTerm
    DUnit : DTerm
    DBracketed :  DTerm -> DTerm
    DTermNotImplemented : String -> DTerm
    
  ||| DClause is a reduced version of PClause.
  public export  
  data DClause : Type where
    MkDPatClause : (lhs : DTerm) -> (rhs : DTerm) -> DClause
    MkDClauseNotImplemented : String -> DClause


  ||| Reduced vaersion of DTypeDecl
  public export   
  data DTypeDecl : Type where
    MkDTy : (n : APIDef.Name) -> (doc : String) -> (type : DTerm) -> DTypeDecl

  ||| Reduced vaersion of DDataDecl
  public export
  data DDataDecl : Type where
    MkDData : (tyname : APIDef.Name) -> (tycon : DTerm) -> (datacons : List DTypeDecl) -> DDataDecl
--    MkDLater : (tyname : APIDef.Name) -> (tycon : DTerm) -> DDataDecl
    MkDDataNotImplemented : String -> DDataDecl

Show Namespace where
  show (DMkNS xs) = ("DMkNS:" ++ show xs)

Show ModuleIdent where
  show (DMkMI xs) = ("DMkMI:" ++ show xs)


mutual
  export
  Show DTerm where
    show (DRef x) = x
    show (DPi Nothing argTy retTy) = show argTy ++ "->" ++ show retTy
    show (DPi (Just x) argTy retTy) = "Π" ++ x ++ ":" ++ show argTy ++ "->" ++ show retTy
    show (DLam x argTy scope) = parens("λ" ++ show x ++ "." ++ show argTy ++ "-->" ++ show scope)
    show (DApp x y) = show x ++ parens( show y)
    show (DNamedApp x name y) = show x ++ show name ++ show y
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
--    show (DList xs) = "List ";
--    show (DPair x y) = "(" ++ show x ++ "," ++ show y ++ ")"
    show DUnit = "()"
    show (DBracketed x) = show x
    show (DTermNotImplemented msg) = "Not supported Term:" ++ msg


  ||| Data field of record.
  export
  Show DField where
    show (MkDField d x ty) = x ++ ":" ++ (show ty)

  export 
  Show DClause where
    show (MkDPatClause lhs rhs) = show lhs ++ ":=" ++ show rhs
    show (MkDClauseNotImplemented x) = "Not supported"

  export
  Show DTypeDecl where
    show (MkDTy n doc type) = n ++ ":" ++ show type

  export
  Show DDataDecl where
    show (MkDData tyname tycon datacons) = tyname ++ ":" ++ show tycon ++ " " ++ (sconcat " " $ map show datacons)
--    show (MkDLater tyname tycon) = tyname ++ ":" ++ show tycon
    show (MkDDataNotImplemented x) = "MkDDataNotImplemented:" ++ x

  public export
  Show DDecl where
    show (DClaim x) = "DClaim:" ++ show x
    show (DDef xs) = "DDef:" ++ (sconcat " " $ map show xs)
    show (DData doc x) = "DData:" ++ show x
    show (DRecord doc n con fs) = "DRecord:" ++ show n ++ " " ++ show con ++ " " ++  show fs
    show (DNamespace n ds) = "DNamespace:" ++ show n ++ " " ++ show ds
    show (DDeclNotImplemented msg) = "DDeclNotImplemented:" ++ "Not implemented:" ++ msg

prettyMaybe : (a -> Doc ann) -> Maybe a -> Doc ann
prettyMaybe _ Nothing = pretty "Nothing"
prettyMaybe f (Just a) = pretty "Just" <++> f a

p : Doc ann -> Doc ann
p = parenthesise True
q : String -> Doc ann
q = dquotes . pretty

escape : Char -> String -> String
escape _ a = a

qq : String -> Doc ann 
qq = dquotes . pretty . escape '\''

ms : Maybe String -> Doc ann
ms = prettyMaybe q


Pretty Namespace where
  pretty (DMkNS xs) = p("DMkNS" <++> pretty xs)
Pretty ModuleIdent where
  pretty (DMkMI xs) = p("DMkNS" <++> pretty xs)


mutual
  public export
  Pretty DDecl where
    pretty (DClaim x) = p ("DClaim" <++> pretty x)
    pretty (DDef xs) = p ("DDef" <++> pretty xs)
    pretty (DData doc x) = p ("DData" <++> qq doc <++> pretty x)
    pretty (DRecord doc n con fs) = p ("DRecord" <++> qq doc <++> q n <++> ms con <++> pretty fs)
    pretty (DNamespace ns ds) = p ("DNamespace" <++> pretty ns <++> pretty ds)

    pretty (DDeclNotImplemented x) = p ("DDeclNotImplemented" <++> qq x)
  export
  Pretty DDataDecl where
    pretty (MkDData tyname tycon datacons) = p ("MkDData" <++> q tyname <++> pretty tycon <++> pretty datacons)
--    pretty (MkDLater tyname tycon) = p ("MkDLater" <++> q tyname <++> pretty tycon)
    pretty (MkDDataNotImplemented x) = p ("MkDDataNotImplemented" <++> qq x)
  export
  Pretty DTypeDecl where
    pretty (MkDTy n doc type) = p ("MkDTy" <++> q n <++> qq doc <++> pretty type)
  export
  Pretty DClause where
    pretty (MkDPatClause lhs rhs) = p ("MkDPatClause" <++> pretty lhs <++> pretty rhs)
    pretty (MkDClauseNotImplemented x) = p ("MkDClauseNotImplemented" <++> qq x)
  export
  Pretty DField where
    pretty (MkDField doc x ty) = p ("MkDField" <++> qq doc <++> q x <++> pretty ty)
  export
  Pretty DTerm where
    pretty (DRef x) = p ("DRef" <++> q x)
    pretty (DPi x argTy retTy) 
      = p ("DPi" <++> p (ms x) <++> pretty argTy <++> pretty retTy)    
    pretty (DLam x argTy scope) = p ("DLam" <++> pretty x <++> pretty argTy <++> pretty scope)
    pretty (DApp x y) = p ("DApp" <++> pretty x <++> pretty y)
    pretty (DNamedApp x n y) = p ("DNamedApp" <++> pretty x <++> q n <++> pretty y)    
    pretty (DPrimVal x) = p ("DPrimVal" <++> pretty x)
    pretty DImplicit = pretty "DImplicit"
    pretty DInfer = pretty "DInfoer"
    pretty (DHole x) = p ("DHole" <++> q x)
    pretty DType = pretty "DType"
    pretty DUnit = pretty "DUnit"
    pretty (DBracketed x) = p ("DBracketed" <++> pretty x)
    pretty (DTermNotImplemented x) = p ("DTermNotImplemented" <++> qq x)
