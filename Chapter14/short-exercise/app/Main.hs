module Main where

import Test.Hspec

multiple :: (Eq a, Num a) => a -> a -> a
multiple _ 0 = 0
multiple x y = x + (multiple x (y - 1))

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1" $ do
      multiple 0 1 == 0 `shouldBe` True
    it "2" $ do
      multiple 1 0 == 0 `shouldBe` True
    it "3" $ do
      multiple 2 8 == 16 `shouldBe` True
