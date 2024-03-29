{-# LANGUAGE OverloadedStrings #-}

module App where

import Control.Concurrent.MVar (MVar, newMVar)
import Network.Wai (Request, Response, pathInfo)
import Network.Wai.Handler.Warp (run)

import Game
import Handlers

app :: MVar GameState -> Request -> (Response -> IO a) -> IO a
app gs req respond = case pathInfo req of
  ["style.css"] -> respond cssHandler
  ["mark", pos] -> markHandler gs pos respond
  ["restart"] -> restartHandler gs respond
  ["end", result] -> endHandler result respond
  _ -> mainHandler gs respond

startApp :: IO ()
startApp = do
  gameState <- newMVar $ GameState (replicate 9 Nothing) X
  run 80 $ app gameState
