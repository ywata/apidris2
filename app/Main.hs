module Main where

import Data.Maybe
import Text.PrettyPrint

import Language.APIDef
import Language.APIDef.Utils
import Language.Haskell
import API ( apiDef )
import Language.APIDef.PrettyPrint
{-import Language.ProtocolBuffers.Types
import Language.ProtocolBuffers.Parser
import Language.ProtocolBuffers.PrettyPrint

main :: IO()
main = do
  rs <- parseProtoBufFile "test/proto/addressbook.proto"
  case rs of
    Right rs -> do
      print rs
      putStrLn . render $ pretty rs
    Left rs -> print rs
-}


main :: IO ()
main = do
  putStrLn "DClaim"
{-  let claims = map isAPITypeDecl $ filter isDClaim apiDef
      datadefs = filter isDData apiDef
      records = filter isDRecord apiDef
  print claims
  print $ map (maybe Nothing (\(_, i, o) -> Just (termName i, termName o))) claims
  let user = map (namedType "User") apiDef
      date = map (namedType "Date") apiDef
  putStrLn . render $ pretty user
  putStrLn . render $ pretty date
-}
  
  pure ()

