module Main (main) where

import Test.Hspec

import Data.Tokenizer.BPE

main :: IO ()
main = hspec $ do
  describe "tiktoken-hs" $ do
    it "returns the correct number of tokens" $ do
      bpeCountTokens 0 `shouldBe` 10
