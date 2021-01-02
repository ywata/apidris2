module Language.APIDef

import Idris.Syntax as IS
import Core.TT as CT
import Core.Name as CN
import Data.List

public export
Name : Type
Name = String

{-
As source AST of Idris2 is too big to describe API.
To minimize efforts, reduced version of AST is defined.
Because of its reduction, if we just remove unnecessary definition, some functions becomes partial,
we choose to define NotIMplemented definition for such reduced definitions.

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


constantToConst: Constant -> Const
constantToConst (I x) =     (CI x)
constantToConst (BI x) =    (CBI x)
constantToConst (B8 x) =    (CB8 x)
constantToConst (B16 x) =   (CB16 x)
constantToConst (B32 x) =   (CB32 x)
constantToConst (B64 x) =   (CB64 x)
constantToConst (Str x) =   (CStr x)
constantToConst (Ch x) =    (CCh x)
constantToConst (Db x) =    (CDb x)
constantToConst WorldVal =  CWorldVal
constantToConst IntType =   CIntType
constantToConst IntegerType = CIntegerType
constantToConst Bits8Type = CBits8Type
constantToConst Bits16Type = CBits16Type
constantToConst Bits32Type = CBits32Type
constantToConst Bits64Type = CBits64Type
constantToConst StringType = CStringType
constantToConst CharType = CCharType
constantToConst DoubleType = CDoubleType
constantToConst WorldType = CWorldType

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

(+-+) : String -> String -> String
(+-+) l r = l ++ " " ++ r

export
sconcat : String -> List String -> String
sconcat sep xs = concat $ intersperse sep xs

-- Restricted version of Source syntax

mutual
  ||| DDecl is a reduced version of PDecl.
  ||| Anything ommited becomes DDeclNotIMplemented.
  public export
  data DDecl : Type where
    DClaim : DTypeDecl -> DDecl
    DDef : List DClause -> DDecl
    DData : DDataDecl -> DDecl
    DRecord : APIDef.Name -> (params : List APIDef.Name) -> (conName : Maybe APIDef.Name) -> List DField -> DDecl
    DMutual : List DDecl -> DDecl
    DDeclNotImplemented: String -> DDecl

  ||| DField is a reduced version of PDField.
  data DField : Type where
    MkDField : APIDef.Name -> (ty : DTerm) -> DField

  ||| DTerm is a reduced version of PTerm.
  ||| This is one of the most central pert of APIDef.
  ||| Selected terms are all necessary to define data type, record, muti-argument function definitions and etc.
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
  data DClause : Type where
    MkDPatClause : (lhs : DTerm) -> (rhs : DTerm) -> DClause
    MkDClauseNotSupported : String -> DClause
   
  data DTypeDecl : Type where
    MkDTy : (n : APIDef.Name) -> (type : DTerm) -> DTypeDecl

  data DDataDecl : Type where
    MkDData : (tyname : APIDef.Name) -> (tycon : DTerm) -> (datacons : List DTypeDecl) -> DDataDecl
    MkDataDeclNotSUpported : String -> DDataDecl


  convertName : CN.Name -> APIDef.Name
  convertName = show


  convertTerm : IS.PTerm -> DTerm
  convertTerm (PRef fc x) = DRef (convertName x)
  convertTerm (PPi fc x y z argTy retTy) = DPi (convertName <$> z) (convertTerm argTy) (convertTerm retTy)
  convertTerm (PLam fc x y z argTy scope) = DLam (convertTerm z) (convertTerm argTy) (convertTerm scope)
  convertTerm p@(PLet fc x pat nTy nVal scope alts) = DTermNotSupported (show p)
  convertTerm p@(PCase fc x xs) = DTermNotSupported ("PCase")
  convertTerm p@(PLocal fc xs scope) = DTermNotSupported ("PLocal")
  convertTerm p@(PUpdate fc xs) = DTermNotSupported ("PUpdate")
  convertTerm p@(PApp fc x y) = DApp (convertTerm x) (convertTerm y)
  convertTerm p@(PWithApp fc x y) = DTermNotSupported ("PWithApp")
  convertTerm p@(PNamedApp fc x y z) = DTermNotSupported ("NamedApp")
  convertTerm p@(PAutoApp fc x y) = DTermNotSupported ("PAutoApp")
  convertTerm p@(PDelayed fc x y) = DTermNotSupported ("PDelayed")
  convertTerm p@(PDelay fc x) = DTermNotSupported ("PDelay")
  convertTerm p@(PForce fc x) = DTermNotSupported ("PForce")
  convertTerm p@(PSearch fc depth) = DTermNotSupported ("PSearch")
  convertTerm p@(PPrimVal fc x) = DPrimVal (constantToConst x)
  convertTerm p@(PQuote fc x) = DTermNotSupported ("PQuote")
  convertTerm p@(PQuoteName fc x) = DTermNotSupported ("PQuoteName")
  convertTerm p@(PQuoteDecl fc xs) = DTermNotSupported ("PQuoteDecl")
  convertTerm p@(PUnquote fc x) = DTermNotSupported ("PUnquote")
  convertTerm p@(PRunElab fc x) = DTermNotSupported ("PRunElab")
  convertTerm p@(PHole fc bracket holename) = DHole (show holename)
  convertTerm p@(PType fc) = DType
  convertTerm p@(PAs fc x pattern) = DTermNotSupported ("PAs")
  convertTerm p@(PDotted fc x) = DTermNotSupported ("PDotted")
  convertTerm p@(PImplicit fc) = DImplicit
  convertTerm p@(PInfer fc) = DInfer
  convertTerm p@(POp fc x y z) = DTermNotSupported ("POp")
  convertTerm p@(PPrefixOp fc x y) = DTermNotSupported ("PPrefixOp")
  convertTerm p@(PSectionL fc x y) = DTermNotSupported ("PSectionL")
  convertTerm p@(PSectionR fc x y) = DTermNotSupported ("PSectionR")
  convertTerm p@(PEq fc x y) = DTermNotSupported ("PEq")
  convertTerm p@(PBracketed fc x) = DBracketed (convertTerm x)
  convertTerm p@(PDoBlock fc x xs) = DTermNotSupported ("PDoBlock")
  convertTerm p@(PBang fc x) = DTermNotSupported ("PBang")
  convertTerm p@(PIdiom fc x) = DTermNotSupported ("PIdiom")
  convertTerm p@(PList fc xs) = DTermNotSupported ("PList")
  convertTerm p@(PPair fc x y) = DTermNotSupported ("PPair")
  convertTerm p@(PDPair fc x y z) = DTermNotSupported ("PDPair")
  convertTerm p@(PUnit fc) = DUnit
  convertTerm p@(PIfThenElse fc x y z) = DTermNotSupported ("PIfThenElse")
  convertTerm p@(PComprehension fc x xs) = DTermNotSupported ("PComprehension")
  convertTerm p@(PRewrite fc x y) = DTermNotSupported ("PRewrite")
  convertTerm p@(PRange fc x y z) = DTermNotSupported ("PRange")
  convertTerm p@(PRangeStream fc x y) = DTermNotSupported ("PRangeStream")
  convertTerm p@(PPostfixApp fc x xs) = DTermNotSupported ("PPostfixApp")
  convertTerm p@(PPostfixAppPartial fc xs) = DTermNotSupported ("PPostfixAppPartial")
  convertTerm p@(PUnifyLog fc x y) = DTermNotSupported ("PUnifyLog")
  convertTerm p@(PWithUnambigNames fc xs x) = DTermNotSupported ("PWithUnambigNames")


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


  convertClause : IS.PClause -> DClause
  convertClause (MkPatClause fc lhs rhs whereblock) = MkDPatClause (convertTerm lhs) (convertTerm rhs)
  convertClause (MkWithClause fc lhs wval xs ys) = MkDClauseNotSupported "MKWIthClause"
  convertClause (MkImpossible fc lhs) = MkDClauseNotSupported "MkIMpossible"

  convertField : IS.PField -> DField
  convertField (MkField fc doc x y z ty) = MkDField (convertName z) (convertTerm ty)

  Show DField where
    show (MkDField x ty) = x ++ ":" ++ (show ty)

  export 
  Show DClause where
    show (MkDPatClause lhs rhs) = show lhs ++ ":=" ++ show rhs
    show (MkDClauseNotSupported x) = "Not supported"



  convertTypeDecl : IS.PTypeDecl -> DTypeDecl
  convertTypeDecl (MkPTy fc n doc type) = MkDTy (convertName n) (convertTerm type)
  export
  Show DTypeDecl where
    show (MkDTy n type) = n ++ ":" ++ show type



  convertDataDecl : IS.PDataDecl -> DDataDecl
  convertDataDecl (MkPData fc tyname tycon opts datacons) = MkDData (convertName tyname) (convertTerm tycon) (map convertTypeDecl datacons)
  convertDataDecl (MkPLater fc tyname tycon) = MkDataDeclNotSUpported "MkPLater"

  export
  Show DDataDecl where
    show (MkDData tyname tycon datacons) = tyname ++ ":" ++ show tycon ++ " " ++ (sconcat " " $ map show datacons)
    show (MkDataDeclNotSUpported x) = "Not supported"


  export
  convertDecl : PDecl -> DDecl
  convertDecl p@(PClaim fc x y xs z) = DClaim (convertTypeDecl z)
  convertDecl p@(PDef fc xs) = DDef (map convertClause xs)
  convertDecl p@(PData fc doc x y) = DData (convertDataDecl y)
  convertDecl p@(PRecord fc doc v n ps con fs)  = DRecord (convertName n) [] (convertName <$> con) (map convertField fs)
  convertDecl p@(PMutual fc xs) = DMutual (map convertDecl xs)
  convertDecl p = DDeclNotImplemented ""


  export
  Show DDecl where
    show (DClaim x) = show x
    show (DDef xs) = sconcat " " $ map show xs
    show (DData x) = show x
    show (DRecord x params conName xs) = "record "  ++ x ++ show conName ++ (sconcat " " $ map show params) ++ (sconcat " " $ map show xs)

    show (DMutual xs) = sconcat " " $ map show xs
    show (DDeclNotImplemented msg) = "Not implemented:" ++ msg
