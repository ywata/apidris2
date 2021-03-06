{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
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

import API (apiDefs)
import PlantUMLTools
import APIDefTools
import Language.APIDef.PrettyPrint
import Control.Monad.IO.Class
import qualified Options.Declarative as D


xref :: [(ModuleIdent, [Maybe T.Text])] -> Maybe T.Text ->   [(ModuleIdent, Maybe T.Text)]
xref ds n = concatMap (`xref'` n) ds
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
  let pumlFile = D.get file
  plantUML <- liftIO $ P.parsePlantUMLFile pumlFile
  let pdefs = fmap (fmap (map getMessage . filter isArrowDef))  plantUML
      ddefs = map getNames apiDefs
  liftIO $ uprint $ fmap (fmap $ map (xref ddefs))  pdefs
  liftIO $ uprint $ fmap (fmap $ filter isJust .  map (nxref ddefs)) pdefs
  liftIO $ uprint $ fmap (fmap $ filter isJust .  map getAPI) apiDefs
  liftIO $ uprint $ fmap (fmap $ flip findDefinition "userAPI" ) apiDefs

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
  liftIO $ putStrLn $ show apiDefs

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
main =
  D.run_ $ D.Group "Group of actions"
  [
    D.subCmd "analyze" analyze
  , D.subCmd "check" checkPlantUML
  , D.subCmd "preprocess" preprocess
  , D.subCmd "show" showAPI
  , D.subCmd "showp" showPlantUML
  ]

