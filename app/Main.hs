{-# LANGUAGE DataKinds #-}
module Main where
import System.Exit
import Data.Maybe
import Text.PrettyPrint

import Language.APIDef
import Language.APIDef.Utils
import Language.Haskell

import qualified Language.PlantUML as P

import API 
import Language.APIDef.PrettyPrint
import Control.Monad.IO.Class
import qualified Options.Declarative as D

analyze :: D.Arg "idris file name" FilePath -> D.Cmd "analyze idris file to generate haskell code" ()
analyze file =
    liftIO $ putStrLn $ show (D.get file) ++ "!" 

preprocess :: D.Arg "files" [FilePath] -> D.Cmd "Preprocess PlantUML files" ()
preprocess files =
  liftIO $ putStrLn $ show (D.get files)
  


checkPlantUML :: D.Arg "idris file name" [FilePath] -> D.Cmd "check PlantUML sequence message against API declaration" ()
checkPlantUML files = do
  let fs = D.get files
  go fs
  where
    go [] = liftIO $ die "no file specified"
    go fs = liftIO $ checkPUML fs

checkPUML [] = return ()
checkPUML (f : fs) = do
  ps <- P.parsePlantUMLFile f
  print ps
  checkPUML fs


main :: IO ()
main = do
  D.run_ $ D.Group "Group of actions"
    [
      D.subCmd "analyze" analyze
    , D.subCmd "check" checkPlantUML
    , D.subCmd "preprocess" preprocess
    ]
{-    
  putStrLn "DClaim"
  print apiDef

  let claims = map isAPITypeDecl $ filter isDClaim apiDef
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

