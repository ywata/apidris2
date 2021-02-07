module API where
import Language.APIDef
apiDef :: [DDecl]
apiDef = [ (DClaim (MkDTy "userAPI" "" (DApp (DApp (DRef "API") (DRef "UserId")) (DTermNotImplemented "PPair"))))
         , (DClaim (MkDTy "api2API" "" (DApp (DApp (DRef "API") DUnit) DUnit))) ]
