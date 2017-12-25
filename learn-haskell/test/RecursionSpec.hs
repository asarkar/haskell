module RecursionSpec (main, spec) where

import Test.Hspec
import Recursion

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let input = [3, 1, 5, 2, 4] :: [Int]
      output = [1, 2, 3, 4, 5] :: [Int]
  describe "bubblesort" $ do
    it ("sorts " ++ show input) $ do
      bubblesort input `shouldBe` output

  describe "mergesort" $ do
    it ("sorts " ++ show input) $ do
      mergesort input `shouldBe` output

  describe "quicksort" $ do
      it ("sorts " ++ show input) $ do
        quicksort input `shouldBe` output