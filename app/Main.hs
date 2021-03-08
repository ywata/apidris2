{-# LANGUAGE DataKinds #-}
module Main where
import Control.Monad (join)
import System.Exit
import Data.Maybe
import qualified Data.Text as T hiding(map, concatMap)
import Text.PrettyPrint

import Text.Show.Unicode (uprint)

import Language.APIDef
import Language.APIDef.Utils
import Language.Haskell

import qualified Language.PlantUML as P

import API (apiDef)
import PlantUMLTools
import APIDefTools
import Language.APIDef.PrettyPrint
import Control.Monad.IO.Class
import qualified Options.Declarative as D


xref :: [(ModuleIdent, [Maybe T.Text])] -> Maybe T.Text ->   [(ModuleIdent, Maybe T.Text)]
xref ds n = concatMap (flip xref' n) ds
  where
    xref' :: (ModuleIdent, [Maybe T.Text]) -> Maybe T.Text ->  [(ModuleIdent, Maybe T.Text)]
    xref' (mi, ds) n = zip (repeat mi) (filter (n ==) ds)


nxref :: [(ModuleIdent, [Maybe T.Text])] -> Maybe T.Text -> Maybe T.Text
nxref ds n = case found of
  [] -> join $ Just n
  _ -> Nothing
  where
    names = concatMap snd ds
    found = filter (n ==) names

analyze :: D.Arg "PlantUML file name" FilePath -> D.Cmd "analyze PlantUML to compair against API.hs" ()
analyze file = do
  let pumlFile = (D.get file)
  plantUML <- liftIO $ P.parsePlantUMLFile pumlFile
  let pdefs = fmap (fmap (map getMessage . filter isArrowDef))  plantUML
      ddefs = map getNames $ apiDef
  liftIO $ uprint $ fmap (fmap $ map (xref ddefs))  $ pdefs
  liftIO $ uprint $ fmap (fmap $ filter isJust .  map (nxref ddefs))  $ pdefs
  liftIO $ uprint $ fmap (fmap $ filter isJust .  map getAPI)  $ apiDef

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

showAPI :: D.Cmd "show API definition in Haskell" ()
showAPI =
  liftIO $ putStrLn $ show apiDef

showPlantUML :: D.Arg "Show PlantUML file" FilePath -> D.Cmd "show PlantUML file" ()
showPlantUML file = do
  let f = D.get file
  go f
  where
    go file = liftIO $ do
      pres <- P.parsePlantUMLFile file
      case pres of
        Right grm -> print grm
        _ -> print "parse err"
        

main :: IO ()
main = do
  D.run_ $ D.Group "Group of actions"
    [
      D.subCmd "analyze" analyze
    , D.subCmd "check" checkPlantUML
    , D.subCmd "preprocess" preprocess
    , D.subCmd "show" showAPI
    , D.subCmd "showp" showPlantUML
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

