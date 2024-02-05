module GameSpec where

import Test.Hspec
import Control.Exception (evaluate)

import Game

spec :: Spec
spec = do
  describe "check isWin function" $ do
    it "checks primary diagonal" $ do
      isWin [Just X,  Nothing, Nothing,
             Nothing, Just X,  Nothing,
             Nothing, Nothing,  Just X] X `shouldBe` True
    it "checks secondary diagonal" $ do
      isWin [Nothing, Nothing, Just X,
             Nothing, Just X,  Nothing,
             Just X,  Nothing, Nothing] X `shouldBe` True
  describe "check makeMove function" $ do
    it "checks putting X to the first square" $ do
      makeMove (GameState (replicate 9 Nothing) X) 0 `shouldBe` GameState (Just X : replicate 8 Nothing) O
