{-# LANGUAGE OverloadedStrings #-}
module AppSpec where

import Control.Concurrent.MVar (MVar)
import Test.Hspec
import Test.Hspec.Wai

import App
import Game

spec :: MVar GameState -> Spec
spec gs = do
  with (return $ app gs) $ do
    describe "GET /restart" $ do
      it "returns 303" $ do
        get "/restart" `shouldRespondWith` 303 {matchHeaders = ["Location" <:> "/"]}
    describe "GET /" $ do
      it "returns 200" $ do
        get "/" `shouldRespondWith` 200
