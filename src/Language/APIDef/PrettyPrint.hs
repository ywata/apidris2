{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.APIDef.PrettyPrint where

import Language.APIDef.APIDef
import Text.PrettyPrint

prettyMaybe :: Pretty a => (a -> Doc) -> Maybe a -> Doc
prettyMaybe _ Nothing = pretty "Nothing"
prettyMaybe f (Just v) = pretty "Just" <+> f v

p :: Doc -> Doc
p = parens
q :: String -> Doc
q = doubleQuotes . pretty

escape :: Char -> String -> String
escape _ a = a

qq :: String -> Doc
qq = doubleQuotes . pretty . escape '\''

ms :: Maybe String -> Doc
ms = prettyMaybe q

class Pretty a where
  pretty :: a -> Doc

instance Pretty String where
  pretty = text




instance Pretty Const where
     pretty (CI i)        = pretty (show i)
     pretty (CBI i)       = pretty (show i)
     pretty (CB8 i)       = pretty (show i)
     pretty (CB16 i)      = pretty (show i)
     pretty (CB32 i)      = pretty (show i)
     pretty (CB64 i)      = pretty (show i)
     pretty (CStr s)      = pretty (show s)
     pretty (CCh c)       = pretty (show c)
     pretty (CDb d)       = pretty (show d)
     pretty (CWorldVal)   = pretty "#WorldVal"
     pretty (CIntType)    = pretty "Int"
     pretty (CIntegerType)  = pretty "Integer"
     pretty (CBits8Type)    = pretty "B8"
     pretty (CBits16Type)   = pretty "B16"
     pretty (CBits32Type)   = pretty "B32"
     pretty (CBits64Type)   = pretty "B64"
     pretty (CStringType)   = pretty "String"
     pretty (CCharType)     = pretty "Char"
     pretty (CDoubleType)   = pretty "Double"
     pretty (CWorldType)    = pretty "#World"

