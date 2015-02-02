{-# LANGUAGE DeriveGeneric, OverloadedStrings, StandaloneDeriving #-}

import Test.Hspec
-- import Test.QuickCheck

import HW08

main :: IO()
main = hspec $ do
  describe "Prelude.heade" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)
  describe "stringFitsFromat" $ do
    it "pass first input" $ do
      stringFitsFormat "3aaa2aa" `shouldBe` True
    it "should fail invalid number" $ do
      stringFitsFormat "3aaa2a" `shouldBe` False
    it "should pass large number(9)" $ do
      stringFitsFormat "9aaaaaaaaa" `shouldBe` True
    it "should not parse two digits(10)" $ do
      stringFitsFormat "10aaaaaaaaaa" `shouldBe` False
    it "should pass zero" $ do
      stringFitsFormat "0" `shouldBe` True
    it "should not pass one" $ do
      stringFitsFormat "1" `shouldBe` False
    it "should pass prefixPadding 0s" $ do
      stringFitsFormat "001a" `shouldBe` True
    it "should fail invalid number padded after 0s" $ do
      stringFitsFormat "100a" `shouldBe` False
    it "should pass simple input with 'a's" $ do
      stringFitsFormat "2aa2aa" `shouldBe` True
    it "should fail simple input with 'b's" $ do
      stringFitsFormat "2bb2bb" `shouldBe` False 
