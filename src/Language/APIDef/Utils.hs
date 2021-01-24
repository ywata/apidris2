module Language.APIDef.Utils (apiInOut
                             , isAPITypeDecl
                             , isDClaim
                             , isDDef
                             , isDData
                             , isDRecord
                             , isDNotImplemented
                             , termName
                             , namedType
                             ) where

import Language.APIDef.APIDef 

isNonFunctionalType :: DTerm -> Bool
isNonFunctionalType p@DUnit = True
isNonFunctionalType p@(DBracketed (DRef _)) = True
isNonFunctionalType p@(DBracketed (DApp _ _ )) = True
isNonFunctionalType p@(DBracketed (DPrimVal _)) = True
isNonFunctionalType _ = False

-- | type is named API in out type
isAPITypeDecl :: DDecl -> Maybe (Name, DTerm, DTerm)
isAPITypeDecl (DClaim (MkDTy name doc dt@(DApp (DApp (DRef "API") i) o))) = Just (name, i, o)
isAPITypeDecl _ = Nothing

-- | apiInOut digs structure of DTerm to see if the term matches to the valid API structure.
apiInOut :: Name -> DTerm -> Maybe (DTerm, DTerm)
apiInOut name (DApp (DApp (DRef n) input) output) = if name == n && isNonFunctionalType input && isNonFunctionalType output then
                                                     Just (input, output)
                                                   else Nothing
apiInOut _ _ = Nothing

data TypeName = Prim Const | DT DTerm | DD DDecl | None
  deriving(Show, Read)

termName :: DTerm -> TypeName
termName p@(DRef x) = DT p
termName (DPi mbn l _) = termName l
termName (DLam _ _ _) = None
termName (DApp l _) = termName l
termName (DNamedApp l _ _) = termName l
termName (DPrimVal p) = Prim p
termName DImplicit = None
termName DInfer = None
termName p@(DHole _) = DT p
termName DType = DT DType
termName p@(DList _) = DT p
termName p@(DPair _ _) = DT p
termName DUnit = DT DUnit
termName (DBracketed p) = DT p
termName p = None


namedType :: Name -> DDecl -> TypeName
namedType n p@(DData _ (MkDData name _ _)) | n == name = DD p
namedType n p@(DRecord _ name _ _) | n == name = DD p
namedType _ _ = None


isDClaim, isDDef, isDData, isDRecord, isDNotImplemented :: DDecl -> Bool
isDClaim (DClaim _) = True
isDClaim _ = False
isDDef (DDef _) = True
isDDef _ = False
isDData (DData _ _) = True
isDData _ = False
isDRecord (DRecord _ _ _ _) = True
isDRecord _ = False
isDNotImplemented (DDeclNotImplemented _) = True
isDNotImplementedf _ = False



{-
-- Name of functions are not good.
searchLhs :: Name -> DDecl -> Maybe DDecl
searchLhs name p@(DClaim (MkDTy n doc type)) = if name == n then Just p else Nothing
searchLhs name p@(DDef []) = Nothing
searchLhs name p@(DDef ((MkDPatClause lhs rhs) :: xs)) = const p <$> searchTerm name lhs
searchLhs name p@(DDef ((MkDClauseNotImplemented x) :: xs)) = Nothing
searchLhs name p@(DData doc (MkDData tyname tycon datacons)) = Nothing
searchLhs name p@(DData doc (MkDataDeclNotSUpported x)) = Nothing
searchLhs name p@(DDeclNotImplemented x) = Nothing


searchRhs :: Name -> DDecl -> Maybe DDecl
searchRhs name p@(DClaim x) = const p <$> searchTypeDecl name x
searchRhs name p@(DDef xs) = Nothing
searchRhs name p@(DData doc x) = Nothing
searchRhs name p@(DDeclNotImplemented x) = Nothing

searchTypeDecl :: Name -> DTypeDecl -> Maybe DTypeDecl
searchTypeDecl name p@(MkDTy n doc type) = const p <$> searchTerm name type

searchTerm :: Name -> DTerm -> Maybe DTerm
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
searchTerm name p@(DTermNotImplemented x) = Nothing


-}
