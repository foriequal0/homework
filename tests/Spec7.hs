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
    it "should implement stremFromSeed" $ do
      take 5 (streamToList (streamFromSeed ('x' :) "o")) `shouldBe` ["o", "xo", "xxo", "xxxo", "xxxxo" ]
    it "should implement nats" $ do
      take 10 (streamToList nats) `shouldBe` [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
    it "should implement ruler" $ do
      take 16 (streamToList ruler) `shouldBe` [0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4]
