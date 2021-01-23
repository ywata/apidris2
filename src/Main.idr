module Main

import Data.List
import Data.Maybe

import Idris.Syntax
import Idris.Parser

import System.File
import System

import Language.APIDef as APIDef

import Idris.Syntax as IS
import Core.TT as CT
import Core.Name as CN

import Text.PrettyPrint.Prettyprinter
import Text.PrettyPrint.Prettyprinter.Util

import Text.PrettyPrint.Prettyprinter.Render.String

showConst : Constant -> String
showConst (I x) = "I "
showConst (BI x) = "BI "
showConst (B8 x) = "BB "
showConst (B16 x) = "B26 "
showConst (B32 x) = "B32 "
showConst (B64 x) = "B64 "
showConst (Str x) = "Str "
showConst (Ch x) = "Ch "
showConst (Db x) = "Db "
showConst WorldVal = "WVal "
showConst IntType = "IntT "
showConst IntegerType = "IntegerT "
showConst Bits8Type = "B8T "
showConst Bits16Type = "B16T "
showConst Bits32Type = "B32T "
showConst Bits64Type = "B64T "
showConst StringType = "StringT "
showConst CharType = "CharT "
showConst DoubleType = "DoubleT "
showConst WorldType = "WorldT "

quote : String -> String
quote x = "(" ++ x ++ ")"

len : List a -> Int
len [] = 0
len (x::xs) = 1 + len xs




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

mutual

  convertName : CN.Name -> APIDef.Name
  convertName = show
  -- conversion rule.  PTerm -> DTerm
  export
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
  convertTerm p@(PNamedApp fc x y z) = DNamedApp (convertTerm x) (convertName y) (convertTerm z)
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

  convertClause : IS.PClause -> DClause
  convertClause (MkPatClause fc lhs rhs whereblock) = MkDPatClause (convertTerm lhs) (convertTerm rhs)
  convertClause (MkWithClause fc lhs wval xs ys) = MkDClauseNotSupported "MKWIthClause"
  convertClause (MkImpossible fc lhs) = MkDClauseNotSupported "MkIMpossible"

  convertField : IS.PField -> DField
  convertField (MkField fc doc x y z ty) = MkDField doc (convertName z) (convertTerm ty)


  convertTypeDecl : IS.PTypeDecl -> DTypeDecl
  convertTypeDecl (MkPTy fc n doc type) = MkDTy (convertName n) doc (convertTerm type)


  convertDataDecl : IS.PDataDecl -> DDataDecl
  convertDataDecl (MkPData fc tyname tycon opts datacons)
    = MkDData (convertName tyname) (convertTerm tycon) (map convertTypeDecl datacons)
  convertDataDecl (MkPLater fc tyname tycon) = MkDLater (convertName tyname) (convertTerm tycon)

  export
  convertDecl : PDecl -> DDecl
  convertDecl p@(PClaim fc x y xs z) = DClaim (convertTypeDecl z)
  convertDecl p@(PDef fc xs) = DDef (map convertClause xs)
  convertDecl p@(PData fc doc x y) = DData doc (convertDataDecl y)
  convertDecl p@(PRecord fc doc v n ps con fs)  = DRecord doc (convertName n) (convertName <$> con) (map convertField fs)
  convertDecl p@(PMutual fc xs) = DDeclNotImplemented "desugarDecl removes DMutual"
  convertDecl p = DDeclNotImplemented ""

desugarDecl : PDecl -> List PDecl
desugarDecl (PMutual fc ds) = concatMap desugarDecl ds
desugarDecl p = [p]

{-
  desugarDecl ps (PMutual fc ds)
      = do let mds = mapMaybe (getDecl AsType) ds ++ mapMaybe (getDecl AsDef) ds
           mds' <- traverse (desugarDecl ps) mds
           pure (concat mds')
-}
desugar : PDecl -> List PDecl
desugar = desugarDecl

rule : Grammar Token False Module
rule = prog "()"

moduleToDataDefs : Module -> List APIDef.DDecl
moduleToDataDefs (MkModule headerloc moduleNS imports documentation decls) = map convertDecl . concatMap desugar $ decls


isKnownType : {ty : Type} -> {v : ty} -> (Type, ty) -> Bool
isKnownType (DDecl, y) = True
isKnownType (x, y) = False


hsDef : String -> Doc ann -> Doc ann
hsDef name d = pretty "module API where" <+> hardline <+> 
               pretty "import Language.APIDef" <+> hardline <+>
               pretty name <++> pretty ":: [DDecl]" <+> hardline <+> pretty name <++> equals <++> d


main : IO ()
main = do 
          [name] <- getArgs
             | [] => putStrLn "file name needed"
             | _ :: _ => putStrLn "too many arguments"
          
          let fname = "src/API.idr"
              hsname = "src/API.hs"
          Right contents <- readFile fname
            | Left _ => putStr "File error"
          let Right m = runParser Nothing contents rule
              | Left e => putStrLn ""
          let decls = moduleToDataDefs m
              str = renderString . layoutPretty defaultLayoutOptions . hsDef "apiDef" $ pretty {ann = ()} decls
              
          Right f <- openFile hsname WriteTruncate
            | Left _ => pure ()
          fPutStrLn f str
          closeFile f
          pure ()




tshow : (a : Type) -> (b : a) -> Int
tshow Int y = y
tshow String y = 2
tshow _ _ = 3
