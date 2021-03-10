{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module MessageParserSpec where

import Test.Hspec
import Text.Megaparsec as P
import MessageParser

spec :: Spec
spec = do
  describe "No arg" $ do
    it "test" $  P.parse api "" "abc()"  `shouldBe` (Right (Message "abc" []))
  describe "No arg with space" $ do
    it "test" $  P.parse api "" "abc( )"  `shouldBe` (Right (Message "abc" []))
  describe "Space after expression" $ do
    it "test" $  P.parse api "" "abc() "  `shouldBe` (Right (Message "abc" []))
  describe "One arg" $ do
    it "test" $  P.parse api "" "abc(d)"  `shouldBe` (Right (Message "abc" ["d"]))

  describe "Alphabet" $ do
    it "test" $  P.parse api "" "abc(a, b)"  `shouldBe` (Right (Message "abc" ["a", "b"]))
  describe "Non alphabet" $ do
    it "test" $  P.parse api "" "日本語(漢字,　ひらがな)"  `shouldBe` (Right (Message "日本語" ["漢字", "ひらがな"]))
  describe "Mixed" $ do
    it "test" $  P.parse api "" "a日b本c語d(漢字e,　Fひらがな)"  `shouldBe` (Right (Message "a日b本c語d" ["漢字e", "Fひらがな"]))

  describe "Random spaces" $ do
    it "test" $  P.parse api "" "a日b本c語d ( 漢字e ,　Fひらがな ) "  `shouldBe` (Right (Message "a日b本c語d" ["漢字e", "Fひらがな"]))
      

  describe "A message" $ do
    it "test" $  P.parse api "" "abc"  `shouldBe` (Right (Signal "abc"))
