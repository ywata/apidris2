module APIDefTools where

import qualified Data.Text as T

import Language.APIDef

import Control.Monad

type APIType = (ModuleIdent, [DDecl])

getNames :: APIType -> (ModuleIdent, [Maybe T.Text])
getNames (mi, ds) = (mi, map getName ds)

isDClaim, isDRecord, isDData, isDataDecl :: DDecl -> Bool
isDClaim (DClaim _) = True
isDClaim _ = False
isDRecord (DRecord _ _ _ _) = True
isDRecord _ = False
isDData (DData _ _ ) = True
isDData _ = False
isDataDecl (DRecord _ _ _ _) = True
isDataDecl (DData _ _ ) = True
isDataDecl _ = False

isNamedDClaim, isNamedDRecord, isNamedDData, isNamedDataDecl :: Name -> DDecl -> Bool
isNamedDClaim n (DClaim (MkDTy name _ _)) = n == name
isNamedDClaim _ _ = False
isNamedDRecord n (DRecord _ name _ _) = n == name
isNamedDRecord _ _ = False
isNamedDData n (DData _ (MkDData name _ _)) = n == name
isNamedDData _ _ = False
isNamedDataDecl n (DRecord _ name _ _) = n == name
isNamedDataDecl n (DData _ (MkDData name _ _ )) = n == name
isNamedDataDecl _ _ = False

isAPI :: DDecl -> Bool
isAPI (DClaim (MkDTy _ _ (DApp (DApp _ _) _))) = False
isAPI _ = False

isAPIName :: Name -> DDecl -> Bool
isAPIName n (DClaim (MkDTy name _ (DApp (DApp  _ _) _))) = n == name
isAPIName _ _ = False

getName :: DDecl -> Maybe Name
getName (DClaim (MkDTy name _ _)) = Just name
getName (DRecord _ name _ _) = Just name
getName (DData _ (MkDData name _ _)) = Just name
getName _ = Nothing



