module API where
import Language.APIDef
apiDef :: [DDecl]
apiDef = [ (DData "" (MkDData "API" (DPi (Nothing) DType (DPi (Nothing) DType DType)) [ (MkDTy "Api" "" (DPi (Just "input") DType (DPi (Just "output") DType (DApp (DApp (DRef "API") (DRef "input")) (DRef "output"))))) ]))
         , (DMutual [ (DData "" (MkDData "A" DType [ (MkDTy "A1" "" (DPi (Nothing) (DPrimVal CStringType) (DRef "A")))
                                                   , (MkDTy "A2" "" (DPi (Nothing) (DPrimVal CIntType) (DRef "A"))) ]))
                    , (DData "" (MkDData "IsA1" (DPi (Nothing) (DRef "A") DType) [ (MkDTy "IA1" "" (DPi (Just "s") DImplicit (DApp (DRef "IsA1") (DBracketed (DApp (DRef "A1") (DRef "s")))))) ])) ]) ]
