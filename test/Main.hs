{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Hspec

import Data.Tokenizer.BPE as BPE

main :: IO ()
main = hspec $ do
  describe "tiktoken-hs" $ do
    it "returns the correct number of tokens" $ do
      BPE.countTokens "This is a test         with a lot of spaces" `shouldBe` 10
      BPE.countTokens "Hello my friend it has been a while" `shouldBe` 8
      BPE.countTokens "The GPT family of models process text using tokens, which are common sequences of characters found in text. The models understand the statistical relationships between these tokens, and excel at producing the next token in a sequence of tokens." `shouldBe` 44
