module Main

import Idris.Syntax
import Idris.Parser

import System.File

import ParseSource
import APIDef

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

rule : Grammar Token False Module
rule = prog "()"

moduleToDataDefs : Module -> List APIDef.DDecl
moduleToDataDefs (MkModule headerloc moduleNS imports documentation decls) = map convertDecl decls


main : IO ()
main = do 
          let fname = "src/API.idr"
          Right contents <- readFile fname
            | Left _ => putStr "File error"
          let Right m = runParser Nothing contents rule
              | Left e => putStrLn $ show e

          putStrLn . sconcat "\n" $ map show $ moduleToDataDefs m
          pure ()


