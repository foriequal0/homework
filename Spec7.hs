{-# LANGUAGE DeriveGeneric, OverloadedStrings, StandaloneDeriving #-}

import Test.Hspec
-- import Test.QuickCheck

import HW07

main :: IO()
main = hspec $ do
  describe "Prelude.heade" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

  describe "fibonaccia" $ do
    it "6 ->  8" $ do
      fib 6 `shouldBe` 8
    it "10 -> 15" $ do
      fib 10 `shouldBe` 55

  describe "fibs1" $ do
    it "start with 0,1,1,2,3,5,8,13,21,34,55,89,144,233,377" $ do
      take 15 fibs1 `shouldBe` [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377]

  describe "fibs2" $ do
    it "is same with fibs1" $ do
      take 15 fibs1 `shouldBe` take 15 fibs2

  describe "Stream" $ do
    it "can show 20 variable" $ do
      show [(1 :: Integer),1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1] `shouldBe` show (streamRepeat (1 :: Integer))
