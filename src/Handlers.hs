{-# LANGUAGE OverloadedStrings #-}

module Handlers where

import Control.Concurrent.MVar (MVar, modifyMVar, readMVar)
import Network.Wai             (Response, responseFile, responseLBS)
import Network.HTTP.Types      (status200, status303)
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Text.Read            as T
import qualified Data.Text                 as T

import Game

cssHandler :: Response
cssHandler = responseFile status200 [("Content-Type", "text/css")] "static/style.css" Nothing

markHandler :: MVar GameState -> T.Text -> (Response -> IO b) -> IO b
markHandler gs pos respond = do
  modifyMVar gs $ \oldGameState -> do
    let newGameState = case T.decimal pos of
          Left _ -> oldGameState
          Right (parsedPosition, _) -> makeMove oldGameState parsedPosition where

    resp <-
      respond $
        responseLBS
          status303
          [ case checkWin (board newGameState) of
              Nothing -> ("Location", "/")
              Just Draw -> ("Location", "/end/draw")
              Just (Winner X) -> ("Location", "/end/X")
              Just (Winner O) -> ("Location", "/end/O")
          ]
          ""

    return (newGameState, resp)

restartHandler :: MVar GameState -> (Response -> IO b) -> IO b
restartHandler gs respond = do
  modifyMVar gs $ \_ -> do
    resp <- respond $ responseLBS status303 [("Location", "/")] ""
    return (GameState (replicate 9 Nothing) X, resp)

endHandler :: T.Text -> (Response -> IO b) -> IO b
endHandler result respond = do
  file <- readFile "static/index.html"
  let ls = lines file
  let rendered =
        concat $
          take 9 ls
            ++ [ "<h1>",
                 if result == "draw" then "Ничья!" else T.unpack result ++ " выиграл!",
                 "</h1>",
                 "<a href=\"/restart\"><button class=\"btn\">Новая игра</button></a>"
               ]
            ++ drop 14 ls

  respond $ responseLBS status200 [("Content-Type", "text/html")] $ BLU.fromString rendered

mainHandler :: MVar GameState -> (Response -> IO b) -> IO b
mainHandler gs respond = do
  file <- readFile "static/index.html"
  let ls = lines file
  gameState <- readMVar gs
  let rendered = concat $ take 13 ls ++ renderBoard (board gameState) ++ drop 13 ls where

  respond $ responseLBS status200 [("Content-Type", "text/html")] $ BLU.fromString rendered
