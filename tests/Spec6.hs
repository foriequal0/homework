{-# LANGUAGE DeriveGeneric, OverloadedStrings, StandaloneDeriving #-}

import Test.Hspec
import Data.Aeson
-- import Test.QuickCheck
import Control.Exception (evaluate)
import Data.Monoid

import HW06

deriving instance Eq Market

main :: IO()
main = hspec $ do
  describe "Prelude.heade" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

  describe "ynToBool" $ do
    it "Y to True" $ do
      ynToBool (String "Y") `shouldBe` (Bool True)
    it "N to False" $ do
      ynToBool (String "N") `shouldBe` (Bool False)
    it "other values does not change" $ do
      ynToBool (String "XXN") `shouldBe` (String "XXN")
    it "Can change string in array" $ do
      fmap ynToBool (decode "[\"Y\"]") `shouldBe` (decode "[true]")

  describe "parseData" $ do
    it "same with eitherDecode when not using Y or N" $ do
      parseData "[false]" `shouldBe` eitherDecode "[false]"
    it "Change Y to true" $ do
      parseData "[\"Y\"]" `shouldBe` eitherDecode "[true]"
    it "Doesn change other than \"Y\" or, etc." $ do
      parseData "[\"Regular ordinary text\"]" `shouldBe` eitherDecode "[\"Regular ordinary text\"]"

  describe "parseMarkets" $ do
    it "Can parse Market from JSON" $ do
      parseMarkets "[{\"marketname\":\"market-name\", \"x\":1.234, \"y\":3.456, \"state\":\"market-state\"}]" `shouldBe` Right ([Market "market-name" 1.234 3.456 "market-state"])
    it "should raises exception when faces ill-formed json" $ do 
      evaluate (parseMarkets "[{\"name\":\"market-name\", \"x\":\"1234\", \"y\":3.456, \"state\":\"market-state\"}]" )`shouldThrow` anyException

  describe "OrdList monoid" $ do
    it "should maintains the ordering invariants" $ do
      (OrdList [2,4,6]) <> (OrdList [1,3,5]) `shouldBe` OrdList [1,2,3,4,5,6]
