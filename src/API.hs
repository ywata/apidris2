module API where
import Language.APIDef
apiDef :: [DDecl]
apiDef = [ (DClaim (MkDTy "f" "" (DPi (Nothing) (DRef "a") (DPi (Nothing) (DRef "b") (DPi (Nothing) (DRef "c") (DPi (Nothing) (DRef "d") (DRef "d")))))))
         , (DDef [ (MkDPatClause (DApp (DApp (DApp (DApp (DRef "f") (DRef "a")) (DRef "b")) (DRef "c")) (DRef "d")) (DRef "d")) ])
         , (DData "" (MkDData "API" (DPi (Nothing) DType (DPi (Nothing) DType DType)) [ (MkDTy "Api" "" (DPi (Just "input") DType (DPi (Just "output") DType (DApp (DApp (DRef "API") (DRef "input")) (DRef "output"))))) ]))
         , (DData "" (MkDData "A" DType [ (MkDTy "A1" "" (DPi (Nothing) (DPrimVal CStringType) (DRef "A")))
                                        , (MkDTy "A2" "" (DPi (Nothing) (DPrimVal CIntType) (DRef "A"))) ]))
         , (DData "" (MkDData "IsA1" (DPi (Nothing) (DRef "A") DType) [ (MkDTy "IA1" "" (DPi (Just "s") DImplicit (DApp (DRef "IsA1") (DBracketed (DApp (DRef "A1") (DRef "s")))))) ]))
         , (DClaim (MkDTy "def" "" (DApp (DRef "IsA1") (DBracketed (DApp (DRef "A1") (DRef "s"))))))
         , (DDef [(MkDPatClause (DRef "def") (DRef "IA1"))])
         , (DClaim (MkDTy "Name" "" DType))
         , (DDef [(MkDPatClause (DRef "Name") (DPrimVal CStringType))])
         , (DData "" (MkDData "Id" (DPi (Just "ty") DType (DPi (Nothing) DType DType)) [ (MkDTy "Id'" "" (DPi (Nothing) (DRef "rep") (DApp (DRef "Id") (DRef "rep")))) ]))
         , (DRecord "" "Date" Nothing [ (MkDField "" "year" (DPrimVal CIntType))
                                      , (MkDField "" "month" (DPrimVal CIntType))
                                      , (MkDField "" "day" (DPrimVal CIntType)) ])
         , (DData "" (MkDData "User" DType [ (MkDTy "U1" "" (DPi (Nothing) (DApp (DRef "Id") (DPrimVal CStringType)) (DPi (Nothing) (DRef "Name") (DPi (Nothing) (DRef "Date") (DRef "User"))))) ]))
         , (DClaim (MkDTy "UserId" "" DType))
         , (DDef [ (MkDPatClause (DRef "UserId") (DApp (DNamedApp (DRef "Id") "ty" (DRef "User")) (DPrimVal CStringType))) ])
         , (DRecord "" "DateTime" Nothing [ (MkDField "" "date" (DRef "Date"))
                                          , (MkDField "" "time" (DPrimVal CIntType)) ])
         , (DClaim (MkDTy "userAPI" "" (DApp (DApp (DRef "API") (DBracketed (DPi (Nothing) (DRef "UserId") (DRef "UserId")))) (DRef "User"))))
         , (DClaim (MkDTy "usersAPI" "" (DApp (DApp (DRef "API") DUnit) (DBracketed (DApp (DRef "List") (DRef "User"))))))
         , (DClaim (MkDTy "listUsersAPI" "" (DApp (DApp (DRef "API") (DBracketed (DApp (DRef "List") (DRef "UserId")))) (DBracketed (DPi (Nothing) (DApp (DRef "List") (DRef "User")) (DRef "User"))))))
         , (DClaim (MkDTy "listUsersAPI'" "" (DApp (DApp (DRef "API") (DBracketed (DPrimVal CStringType))) (DBracketed (DApp (DRef "List") (DRef "User"))))))
         , (DClaim (MkDTy "listUserbyNameAPI" "" (DApp (DApp (DRef "API") (DBracketed (DRef "Name"))) (DBracketed (DApp (DRef "List") (DRef "User"))))))
         , (DClaim (MkDTy "updateUserAPI" "" (DApp (DApp (DRef "API") (DBracketed (DRef "User"))) DUnit)))
         , (DClaim (MkDTy "primAPI" "" (DApp (DApp (DRef "API") (DPrimVal CStringType)) (DRef "User")))) ]
