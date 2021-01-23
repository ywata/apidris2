module Main where

import Language.APIDef
import API ( apiDef )

main :: IO ()
main = do
  print $ apiDef


