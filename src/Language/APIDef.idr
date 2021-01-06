module Language.APIDef


import Data.Maybe
import Data.List
import Data.Vect

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
--    DList : List DTerm -> DTerm
--    DPair : DTerm -> DTerm -> DTerm
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
    show (DApp x y) = show x ++ quote( show y)
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
    show (DTermNotSupported msg) = "Not supported Term:" ++ msg

  ||| Data field of record.
  export
  Show DField where
    show (MkDField d x ty) = x ++ ":" ++ (show ty)

  export 
  Show DClause where
    show (MkDPatClause lhs rhs) = show lhs ++ ":=" ++ show rhs
    show (MkDClauseNotSupported x) = "Not supported"

  export
  Show DTypeDecl where
    show (MkDTy n doc type) = n ++ ":" ++ show type

  export
  Show DDataDecl where
    show (MkDData tyname tycon datacons) = tyname ++ ":" ++ show tycon ++ " " ++ (sconcat " " $ map show datacons)
    show (MkDataDeclNotSUpported x) = "Not supported"

  public export
  Show DDecl where
    show (DClaim x) = "DClaim:" ++ show x
    show (DDef xs) = "DDef:" ++ (sconcat " " $ map show xs)
    show (DData doc x) = "DData:" ++ show x
    show (DRecord doc x params conName xs) 
      = "DRecord:"  ++ x ++ show conName ++ (sconcat " " $ map show params) ++ (sconcat " " $ map show xs)

    show (DMutual xs) = sconcat "\n" $ map show xs
    show (DDeclNotImplemented msg) = "DDeclNotImplemented:" ++ "Not implemented:" ++ msg

type1 : {ty : Type} -> (a : ty) -> Type
type1 a = ty

valType : {ty : Type} -> (a : ty) -> (Type, ty)
valType a = (ty, a)

public export
interface Searchable s where
  search' : {ty: Type} -> Name -> s ->  Maybe (Type, ty) -- whree ty

test :  {ty:Type} -> (a : ty) -> Maybe ty
test a = Just a

nonFunctionalType : DTerm -> Bool
nonFunctionalType (DRef x) = ?nonFunctionalType_rhs_1
nonFunctionalType (DPi x argTy retTy) = ?nonFunctionalType_rhs_2
nonFunctionalType (DLam x argTy scope) = ?nonFunctionalType_rhs_3
nonFunctionalType (DApp x y) = ?nonFunctionalType_rhs_4
nonFunctionalType (DPrimVal x) = ?nonFunctionalType_rhs_5
nonFunctionalType DImplicit = ?nonFunctionalType_rhs_6
nonFunctionalType DInfer = ?nonFunctionalType_rhs_7
nonFunctionalType (DHole x) = ?nonFunctionalType_rhs_8
nonFunctionalType DType = ?nonFunctionalType_rhs_9
--nonFunctionalType (DList _) = ?nonFunctionalType_rhs_13
--nonFunctionalType (DPair _ _) = ?nonFunctionalType_rhs_14
nonFunctionalType DUnit = ?nonFunctionalType_rhs_10
nonFunctionalType (DBracketed x) = ?nonFunctionalType_rhs_11
nonFunctionalType (DTermNotSupported x) = ?nonFunctionalType_rhs_12



dig : DTerm -> Maybe (DTerm, DTerm)
dig (DRef x) = Nothing
dig (DPi x argTy retTy) = Nothing
dig (DLam x argTy scope) = Nothing

-- Allowed API
dig (DApp (DApp (DRef "API") y@(DPrimVal _)) a) = Just (y, a)
dig (DApp (DApp (DRef "API") y@(DRef _)) a) = Just (y, a)
dig (DApp (DApp (DRef "API") y@(DBracketed (DApp _ _))) a) = Just (y, a)
dig (DApp (DApp (DRef "API") y@(DBracketed (DPrimVal _))) a) = Just(y, a)
dig (DApp (DApp (DRef "API") y@DUnit) a) = Just(y, a)



-- Not allowed
dig (DApp (DApp x y) z) = Nothing

--dig (DApp (DApp x y) z) = Just (y, z)
dig (DApp _ z) = Nothing
dig (DPrimVal x) = Nothing
dig DImplicit = Nothing
dig DInfer = Nothing
dig (DHole x) = Nothing
dig DType = Nothing
dig DUnit = Nothing
--dig (DList _) = Nothing
--dig (DPair _ _) = Nothing
dig (DBracketed x) = Nothing
dig (DTermNotSupported x) = Nothing


export
apiInOut : DDecl -> Maybe (DTerm, DTerm)
apiInOut (DClaim (MkDTy n doc (DRef x))) = Nothing
apiInOut (DClaim (MkDTy n doc (DPi x argTy retTy))) = Nothing
apiInOut (DClaim (MkDTy n doc (DLam x argTy scope))) = Nothing
apiInOut (DClaim (MkDTy n doc ap@(DApp x y))) = dig ap
apiInOut (DClaim (MkDTy n doc (DPrimVal x))) = Nothing
apiInOut (DClaim (MkDTy n doc DImplicit)) = Nothing
apiInOut (DClaim (MkDTy n doc DInfer)) = Nothing
apiInOut (DClaim (MkDTy n doc (DHole x))) = Nothing
apiInOut (DClaim (MkDTy n doc DType)) = Nothing
--apiInOut (DClaim (MkDTy n doc (DList _))) = Nothing
--apiInOut (DClaim (MkDTy n doc (DPair _ _))) = Nothing
apiInOut (DClaim (MkDTy n doc DUnit)) = Nothing
apiInOut (DClaim (MkDTy n doc (DBracketed x))) = Nothing
apiInOut (DClaim (MkDTy n doc (DTermNotSupported x))) = Nothing
apiInOut (DDef xs) = Nothing
apiInOut (DData doc x) = Nothing
apiInOut (DRecord doc x params conName xs) = Nothing
apiInOut (DMutual xs) = Nothing
apiInOut (DDeclNotImplemented x) = Nothing

mutual
  -- Name of functions are not good.
  export
  searchLhs : Name -> DDecl -> Maybe DDecl
  searchLhs name p@(DClaim (MkDTy n doc type)) = if name == n then Just p else Nothing
  searchLhs name p@(DDef []) = Nothing
  searchLhs name p@(DDef ((MkDPatClause lhs rhs) :: xs)) = const p <$> searchTerm name lhs
  searchLhs name p@(DDef ((MkDClauseNotSupported x) :: xs)) = Nothing
  searchLhs name p@(DData doc (MkDData tyname tycon datacons)) = Nothing
  searchLhs name p@(DData doc (MkDataDeclNotSUpported x)) = Nothing
  searchLhs name p@(DRecord doc x params conName xs) = Nothing
  searchLhs name p@(DMutual xs) = Nothing -- It is supposed to be flatten before calling searchLhs.
  searchLhs name p@(DDeclNotImplemented x) = Nothing

  export
  searchRhs : Name -> DDecl -> Maybe DDecl
  searchRhs name p@(DClaim x) = const p <$> searchTypeDecl name x
  searchRhs name p@(DDef xs) = Nothing
  searchRhs name p@(DData doc x) = Nothing
  searchRhs name p@(DRecord doc x params conName xs) = Nothing
  searchRhs name p@(DMutual xs) = Nothing
  searchRhs name p@(DDeclNotImplemented x) = Nothing

  searchTypeDecl : Name -> DTypeDecl -> Maybe DTypeDecl
  searchTypeDecl name p@(MkDTy n doc type) = const p <$> searchTerm name type

  searchTerm : Name -> DTerm -> Maybe DTerm
  searchTerm name p@(DRef x) = if name == x then Just p else Nothing
  searchTerm name p@(DPi x argTy retTy) = Nothing
  searchTerm name p@(DLam x argTy scope) = Nothing
  searchTerm name p@(DApp x y) = searchTerm name x
  searchTerm name p@(DPrimVal x) = Nothing -- 
  searchTerm name p@DImplicit = Nothing
  searchTerm name p@DInfer = Nothing
  searchTerm name p@(DHole x) = Nothing
  searchTerm name p@DType = Nothing
--  searchTerm name p@(DList _) = Nothing
--  searchTerm name p@(DPair _ _) = Nothing
  searchTerm name p@DUnit = Nothing
  searchTerm name p@(DBracketed x) = Nothing
  searchTerm name p@(DTermNotSupported x) = Nothing
  

export
flatten : DDecl -> List DDecl
flatten p@(DClaim x) = [p]
flatten p@(DDef xs) = [p]
flatten p@(DData doc x) = [p]
flatten p@(DRecord doc x params conName xs) = [p]
flatten p@(DMutual xs) = concatMap flatten xs
flatten p@(DDeclNotImplemented x) = [p]

{-
isNamedTerm : DTerm -> Bool
isNamedTerm (DRef x) = True
isNamedTerm (DPi x argTy retTy) = isJust x
isNamedTerm (DLam x argTy scope) = False
isNamedTerm (DApp x y) = False
isNamedTerm (DPrimVal x) = False
isNamedTerm DImplicit = False
isNamedTerm DInfer = False
isNamedTerm (DHole x) = False
isNamedTerm DType = False
isNamedTerm DUnit = False
isNamedTerm (DBracketed x) = False
isNamedTerm (DTermNotSupported x) = False

isNamedClause : DClause -> Bool
isNamedClause (MkDPatClause lhs rhs) = isNamedTerm lhs
isNamedClause (MkDClauseNotSupported x) = False


export
interface Nameable s where
  namedItem : s -> List s

export
Nameable DDecl where
  namedItem  p@(DClaim (MkDTy n doc type)) = [p]
  namedItem  p@(DDef []) = []
  namedItem  p@(DDef (x::xs)) with (isNamedClause x)
    namedItem  p@(DDef (x::xs)) | True = [p]
    namedItem  p@(DDef (x::xs)) | _ = [p]

  namedItem  p@(DData doc x) = [p]
  namedItem  p@(DRecord doc x params conName xs) = [p]
  namedItem  p@(DMutual xs) = concatMap namedItem xs
  namedItem  p@(DDeclNotImplemented x) = []

-}



