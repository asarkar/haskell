module ListSpec (main, spec) where

import Test.Hspec
import List
import Control.Exception (evaluate)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let list = [3, 1, 5, 2, 4] :: [Int]
  describe "myLast" $ do
    it "should find the last element of a list" $ do
      myLast list `shouldBe` 4
  describe "myButLast" $ do
    it "should find the last but one element of a list" $ do
      myButLast list `shouldBe` 2
      myButLast ['a'..'z'] `shouldBe` 'y'
  describe "myButLast'" $ do
    it "should find the last but one element of a list" $ do
      myButLast' list `shouldBe` 2
      myButLast' ['a'..'z'] `shouldBe` 'y'
  describe "myButLast''" $ do
    it "should find the last but one element of a list" $ do
      myButLast'' list `shouldBe` 2
      myButLast'' ['a'..'z'] `shouldBe` 'y'
  describe "elementAt" $ do
    it "should find the K'th element of a list" $ do
      let l = [1, 2, 3, 4]
      all (\x -> (elementAt l x) == x) l `shouldBe` True
      evaluate (elementAt l 10) `shouldThrow` anyErrorCall
  describe "myLength" $ do
    it "should find the number of elements of a list" $ do
      myLength list `shouldBe` 5
  describe "myReverse" $ do
    it "should reverse a list" $ do
      myReverse list `shouldBe` reverse list
  describe "isPalindrome" $ do
    it "should find out whether a list is a palindrome" $ do
      isPalindrome ([1, 2, 3] :: [Int]) `shouldBe` False
      isPalindrome "madamimadam" `shouldBe` True
  describe "flatten" $ do
    it "should flatten a nested list structure" $ do
      flatten [Left 1, Right [2, 3]] `shouldBe` ([1, 2, 3] :: [Int])
  describe "compress" $ do
    it "should eliminate consecutive duplicates of list elements" $ do
      compress "aaaabccaadeeee" `shouldBe` "abcade"
  describe "pack" $ do
    it "should pack consecutive duplicates of list elements into sublists" $ do
      pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'] `shouldBe` ["aaaa","b","cc","aa","d","eeee"]
  describe "encode" $ do
    it "should create run-length encoding of a list" $ do
      encode "aaaabccaadeeee" `shouldBe` [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]