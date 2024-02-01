{-# LANGUAGE OverloadedStrings #-}

module App(startApp) where

import Control.Concurrent.MVar  (MVar, newMVar)
import Network.Wai              (pathInfo, Response, Request)
import Network.Wai.Handler.Warp (run)

import Handlers
import Game

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
        
