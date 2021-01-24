module Main where

import Language.APIDef
import Language.APIDef.Utils

import API ( apiDef )

main :: IO ()
main = do
  putStrLn "DClaim"
  print $ map isAPITypeDecl $ filter isDClaim apiDef
  pure ()

