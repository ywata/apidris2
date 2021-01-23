module Language.APIDef.Utils (apiInOut) where

import Language.APIDef.APIDef 

isNonFunctionalType :: DTerm -> Bool
isNonFunctionalType p@DUnit = True
isNonFunctionalType p@(DBracketed (DRef _)) = True
isNonFunctionalType p@(DBracketed (DApp _ _ )) = True
isNonFunctionalType p@(DBracketed (DPrimVal _)) = True
isNonFunctionalType _ = False

-- | type is named API in out type
isAPIType :: DTerm -> Bool
isAPIType (DApp (DApp (DRef "API") _) _) = True
isAPIType _ = False

-- | apiInOut digs structure of DTerm to see if the term matches to the valid API structure.
apiInOut :: Name -> DTerm -> Maybe (DTerm, DTerm)
apiInOut name (DApp (DApp (DRef n) input) output) = if name == n && isNonFunctionalType input && isNonFunctionalType output then
                                                     Just (input, output)
                                                   else Nothing

apiInOut _ _ = Nothing

{-
-- Name of functions are not good.
searchLhs :: Name -> DDecl -> Maybe DDecl
searchLhs name p@(DClaim (MkDTy n doc type)) = if name == n then Just p else Nothing
searchLhs name p@(DDef []) = Nothing
searchLhs name p@(DDef ((MkDPatClause lhs rhs) :: xs)) = const p <$> searchTerm name lhs
searchLhs name p@(DDef ((MkDClauseNotSupported x) :: xs)) = Nothing
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
searchTerm name p@(DTermNotSupported x) = Nothing


-}
