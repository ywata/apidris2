module Main where

import Data.Maybe
import Language.APIDef
import Language.APIDef.Utils

import API ( apiDef )

main :: IO ()
main = do
  putStrLn "DClaim"
  let claims = map isAPITypeDecl $ filter isDClaim apiDef
  print claims
  print $ map (maybe Nothing (\(_, i, o) -> Just (termName i, termName o))) claims

  pure ()
