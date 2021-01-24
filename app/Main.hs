module Main where

import Data.Maybe
import Language.APIDef
import Language.APIDef.Utils

import API ( apiDef )

main :: IO ()
main = do
  putStrLn "DClaim"
  let claims = map isAPITypeDecl $ filter isDClaim apiDef
      datadefs = filter isDData apiDef
      records = filter isDRecord apiDef
--  print claims
--  print $ map (maybe Nothing (\(_, i, o) -> Just (termName i, termName o))) claims
--  putStrLn "datadefs"
--  print datadefs
--  putStrLn "records"
--  print records
  let user = map (namedType "User") apiDef
      date = map (namedType "Date") apiDef
  print user
  print date
  
  pure ()
