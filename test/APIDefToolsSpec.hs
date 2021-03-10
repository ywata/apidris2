{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module APIDefToolsSpec where
import Test.Hspec

spec :: Spec
spec = do
  describe "silly" $ do
    it "test" $ 1 `shouldBe` 1

