module Main
import System.File

import Data.List 
import Data.Strings as S
import Data.String.Extra as S
import Data.Maybe

import Idris.Parser

import Parser.Source as PS

import System.File
import System

import Language.APIDef as APIDef

import Idris.Syntax as IS
import Core.TT as CT
import Core.Name as CN
import Core.Name.Namespace as NS

import Text.PrettyPrint.Prettyprinter as PP
import Text.PrettyPrint.Prettyprinter.Doc as PP
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
  convertTerm p@(PLet fc x pat nTy nVal scope alts) = DTermNotImplemented (show p)
  convertTerm p@(PCase fc x xs) = DTermNotImplemented ("PCase")
  convertTerm p@(PLocal fc xs scope) = DTermNotImplemented ("PLocal")
  convertTerm p@(PUpdate fc xs) = DTermNotImplemented ("PUpdate")
  convertTerm p@(PApp fc x y) = DApp (convertTerm x) (convertTerm y)
  convertTerm p@(PWithApp fc x y) = DTermNotImplemented ("PWithApp")
  convertTerm p@(PNamedApp fc x y z) = DNamedApp (convertTerm x) (convertName y) (convertTerm z)
  convertTerm p@(PAutoApp fc x y) = DTermNotImplemented ("PAutoApp")
  convertTerm p@(PDelayed fc x y) = DTermNotImplemented ("PDelayed")
  convertTerm p@(PDelay fc x) = DTermNotImplemented ("PDelay")
  convertTerm p@(PForce fc x) = DTermNotImplemented ("PForce")
  convertTerm p@(PSearch fc depth) = DTermNotImplemented ("PSearch")
  convertTerm p@(PPrimVal fc x) = DPrimVal (constantToConst x)
  convertTerm p@(PQuote fc x) = DTermNotImplemented ("PQuote")
  convertTerm p@(PQuoteName fc x) = DTermNotImplemented ("PQuoteName")
  convertTerm p@(PQuoteDecl fc xs) = DTermNotImplemented ("PQuoteDecl")
  convertTerm p@(PUnquote fc x) = DTermNotImplemented ("PUnquote")
  convertTerm p@(PRunElab fc x) = DTermNotImplemented ("PRunElab")
  convertTerm p@(PHole fc bracket holename) = DHole (show holename)
  convertTerm p@(PType fc) = DType
  convertTerm p@(PAs fc nfc x pattern) = DTermNotImplemented ("PAs")
  convertTerm p@(PDotted fc x) = DTermNotImplemented ("PDotted")
  convertTerm p@(PImplicit fc) = DImplicit
  convertTerm p@(PInfer fc) = DInfer
  convertTerm p@(POp fc x y z) = DTermNotImplemented ("POp")
  convertTerm p@(PPrefixOp fc x y) = DTermNotImplemented ("PPrefixOp")
  convertTerm p@(PSectionL fc x y) = DTermNotImplemented ("PSectionL")
  convertTerm p@(PSectionR fc x y) = DTermNotImplemented ("PSectionR")
  convertTerm p@(PEq fc x y) = DTermNotImplemented ("PEq")
  convertTerm p@(PBracketed fc x) = DBracketed (convertTerm x)


  convertTerm p@(PString fc ls) = DTermNotImplemented ("PString")  
  convertTerm p@(PMultiline fc i lls) = DTermNotImplemented ("PMultiline")
  convertTerm p@(PDoBlock fc x xs) = DTermNotImplemented ("PDoBlock")  
  convertTerm p@(PBang fc x) = DTermNotImplemented ("PBang")
  convertTerm p@(PIdiom fc x) = DTermNotImplemented ("PIdiom")
  convertTerm p@(PList fc xs) = DList (map convertTerm xs)
  convertTerm p@(PPair fc x y) = DPair (convertTerm x) (convertTerm y)
  convertTerm p@(PDPair fc x y z) = DTermNotImplemented ("PDPair")
  convertTerm p@(PUnit fc) = DUnit
  convertTerm p@(PIfThenElse fc x y z) = DTermNotImplemented ("PIfThenElse")
  convertTerm p@(PComprehension fc x xs) = DTermNotImplemented ("PComprehension")
  convertTerm p@(PRewrite fc x y) = DTermNotImplemented ("PRewrite")
  convertTerm p@(PRange fc x y z) = DTermNotImplemented ("PRange")
  convertTerm p@(PRangeStream fc x y) = DTermNotImplemented ("PRangeStream")
  convertTerm p@(PPostfixApp fc x xs) = DTermNotImplemented ("PPostfixApp")
  convertTerm p@(PPostfixAppPartial fc xs) = DTermNotImplemented ("PPostfixAppPartial")
  convertTerm p@(PUnifyLog fc x y) = DTermNotImplemented ("PUnifyLog")
  convertTerm p@(PWithUnambigNames fc xs x) = DTermNotImplemented ("PWithUnambigNames")

  convertClause : IS.PClause -> DClause
  convertClause (MkPatClause fc lhs rhs whereblock) = MkDPatClause (convertTerm lhs) (convertTerm rhs)
  convertClause (MkWithClause fc lhs wval xs ys) = MkDClauseNotImplemented "MKWIthClause"
  convertClause (MkImpossible fc lhs) = MkDClauseNotImplemented  "MkIMpossible"

  convertField : IS.PField -> DField
  convertField (MkField fc doc x y z ty) = MkDField doc (convertName z) (convertTerm ty)


  convertTypeDecl : IS.PTypeDecl -> DTypeDecl
  convertTypeDecl (MkPTy fc nfc n doc type) = MkDTy (convertName n) doc (convertTerm type)


  convertDataDecl : IS.PDataDecl -> DDataDecl
  convertDataDecl (MkPData fc tyname tycon opts datacons)
    = MkDData (convertName tyname) (convertTerm tycon) (map convertTypeDecl datacons)
--  convertDataDecl (MkPLater fc tyname tycon) = MkDLater (convertName tyname) (convertTerm tycon)
  convertDataDecl p@(MkPLater fc tyname tycon) = MkDDataNotImplemented ("MkPLater:" ++ show tyname)

  convertNamespapce : NS.Namespace -> APIDef.Namespace
  convertNamespapce ns = DMkNS (unsafeUnfoldNamespace ns)
  convertModuleIdent : NS.ModuleIdent -> APIDef.ModuleIdent
  convertModuleIdent mi = DMkMI (unsafeUnfoldModuleIdent mi)
  
  export
  convertDecl : PDecl -> DDecl
  convertDecl p@(PClaim fc x y xs z) = DClaim (convertTypeDecl z)
  convertDecl p@(PDef fc xs) = DDef (map convertClause xs)
  convertDecl p@(PData fc doc x y) = DData doc (convertDataDecl y)
  convertDecl p@(PRecord fc doc v n ps con fs)  = DRecord doc (convertName n) (convertName <$> con) (map convertField fs)
  convertDecl p@(PMutual fc xs) = DDeclNotImplemented "desugarDecl removes DMutual"
  convertDecl p@(PNamespace fc ns pd) = DNamespace (convertNamespapce ns) (map convertDecl pd)
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

moduleToDataDefs : Module ->  List APIDef.DDecl
moduleToDataDefs (MkModule headerloc moduleNS imports documentation decls) = map convertDecl . concatMap desugar $ decls


isKnownType : {ty : Type} -> {v : ty} -> (Type, ty) -> Bool
isKnownType (DDecl, y) = True
isKnownType (x, y) = False


hsDef : String -> PP.Doc ann -> PP.Doc ann
hsDef name d = pretty "module API where" <+> hardline <+> 
               pretty "import Language.APIDef" <+> hardline <+>
               pretty name <++> pretty ":: [(ModuleIdent, [DDecl])]" <+> hardline <+> pretty name <++> equals <++> d


data Path : Type where 
  Abs : List String -> Path
  Rel : List String -> Path
  NoPath : Path

Show Path where
  show (Abs xs) = "Abs:" ++ show xs
  show (Rel xs) = "Rel:" ++ show xs
  show NoPath = "NoPath"

splitBy : (Char -> Bool) -> String -> List String -> List String
splitBy _ "" ls = ls
splitBy p s ls = if length (snd pair) == 0 then (fst pair) :: ls else splitBy p (S.drop 1 (snd pair)) ((fst pair)::ls)
  where
    pair : (String, String)
    pair = break p s

path : String -> Path
path "" = NoPath
path s = if isPrefixOf "/" s then Abs (drop 1 ls) else Rel ls
  where
    pref : Bool
    pref = isPrefixOf "/" s
    ls : List String
    ls = reverse $ splitBy (== '/') s []

chgs : String -> List String -> List String
chgs t [] = []
chgs t [x] = [fst pair ++ "." ++ t]
  where
    pair : (String, String)
    pair = break (== '.') x
chgs t (x :: xs) = x :: chgs t xs
  


changeSuffix : String -> Path -> Path
changeSuffix to (Abs xs) = Abs (chgs to xs)
changeSuffix to (Rel xs) = Rel (chgs to xs)
changeSuffix _ NoPath = NoPath

mkPath : Path -> Maybe String
mkPath (Abs xs) = Just $ "/" ++ concat (intersperse "/" xs)
mkPath (Rel xs) = Just $ concat (intersperse "/" xs)
mkPath NoPath = Nothing

(</>) : Path -> Path -> Path
(</>) p q@(Abs xs) = q
(</>) (Abs ys) (Rel xs) = Abs (ys ++ xs)
(</>) (Rel ys) (Rel xs) = Rel (ys ++ xs)
(</>) NoPath r@(Rel xs) = r
(</>) p NoPath = p



readModule : Path -> Path -> IO (Either String Module)
readModule pref p@(Rel _) = do
  let (Just p1) = mkPath $ pref </> changeSuffix "idr" p
     | _ => pure $ Left "path"
  p1Exists <- exists p1
  let (Just p2) = mkPath $ pref </> changeSuffix "lidr" p
     | _ => pure $ Left "path"
  p2Exists <- exists p2
  let p = if p2Exists then p2 else p1
  Right contents <- do 
                    putStrLn p
                    readFile p
    | Left _ => pure $ Left $ "file read error:"  ++ p
  let Right (m@(MkModule headerloc moduleNS imports documentation decls)) = PS.runParser "" (isLitFile p) contents rule
    | Left e => pure $ Left "API spec format error"

  pure $ Right m


readModule _ _ = pure $ Left "Abs path not supported"

readModules : Path -> List Path -> IO (List(NS.ModuleIdent, List PDecl))
readModules _ [] = pure []
readModules pref (f :: xs) = do
  Right p@(MkModule headerloc ns imports documentation decls) <- readModule pref f
    | Left _ => pure []
  ps <- readModules pref xs
  pure $ (ns, decls) :: ps

{-
writeAST : String -> List DDecl -> IO ()
writeAST file decls = 
  do
    let 
--      val : (List String, List DDecl)
--      val = (unsafeUnfoldModuleIdent moduleNS, moduleToDataDefs m)
      str : String
      str = renderString . layoutPretty defaultLayoutOptions . hsDef "apiDef" $ pretty {ann = ()} decls
    Right f <- do
               putStrLn file
               openFile file WriteTruncate
      | Left _ => pure ()
    fPutStrLn f str
    closeFile f
-}    

convPDecl : (NS.ModuleIdent, List PDecl) -> (APIDef.ModuleIdent, List DDecl)
convPDecl (mi, decls) = (convertModuleIdent mi, map convertDecl . concatMap desugar $ decls)
--convPDecl : (NS.ModuleIdent, List PDecl) -> (List DDecl)
--convPDecl (mi, decls) = (map convertDecl . concatMap desugar $ decls)
--convPDecl : (NS.ModuleIdent, List PDecl) -> (APIDef.ModuleIdent)
--convPDecl (mi, decls) = (convertModuleIdent mi)


main : IO ()
main = do 
          [bin, api_file, hs_api_file]  <- getArgs
            | _ => putStrLn "too many arguments"

          Right contents <- readFile api_file
            | Left _ => putStr $ "Read file error:" ++ api_file
          let Right m@(MkModule headerloc moduleNS imports documentation decls)  = runParser "" Nothing contents (prog "()")
              | Left e => putStrLn "API spec format error"
          let idrisFiles = map (Rel . reverse . unsafeUnfoldModuleIdent . path) imports
          decls <- readModules (Rel ["spec"]) idrisFiles

          let 
            str = renderString . layoutPretty defaultLayoutOptions . hsDef "apiDef" $ pretty {ann = ()} 
                $  map convPDecl decls

          Right f <- do
                     putStrLn hs_api_file
                     openFile hs_api_file WriteTruncate
            | Left _ => putStrLn "open failed"
          Right r <- fPutStrLn f str
            | _ => pure ()
          closeFile f
          pure ()


