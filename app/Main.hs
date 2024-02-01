{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.MVar
import Data.ByteString.Lazy.UTF8 qualified as BLU
import Data.Text.Read qualified as T
import Data.Text qualified as T
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types       (status200, status303)

import Game

app :: MVar GameState -> Request -> (Response -> IO a) -> IO a
app gs req respond = case pathInfo req of
        ["style.css"] -> respond $ responseFile status200 [("Content-Type", "text/css")] "static/style.css" Nothing
        ["mark", a] -> do
            modifyMVar gs $ \old_gs -> do
                let newGameState = case T.decimal a of
                        Left _ -> old_gs
                        Right (parsedPosition, _) -> makeMove old_gs parsedPosition where

                resp <- respond $ responseLBS status303 [case checkWin (board newGameState) of
                    Nothing -> ("Location", "/")
                    Just Draw -> ("Location", "/end/draw")
                    Just (Winner X) -> ("Location", "/end/X")
                    Just (Winner O) -> ("Location", "/end/O")] ""
                                
                return (newGameState, resp)
        ["restart"] -> do
            modifyMVar gs $ \_ -> do
                resp <- respond $ responseLBS status303 [("Location", "/")] "" 
                return (GameState (replicate 9 Nothing) X, resp)
        ["end", a] -> do
            file <- readFile "static/index.html"
            let ls = lines file
            let rendered = concat $ take 9 ls ++ ["<h1>", (if a == "draw" then "Ничья!" else T.unpack a ++ " выиграл!"), "</h1>",
                            "<a href=\"/restart\"><button class=\"styled\">Новая игра</button></a>"] ++ drop 14 ls
            respond $ responseLBS status200 [("Content-Type", "text/html")] $ BLU.fromString rendered
        _ -> do
            file <- readFile "static/index.html"
            let ls = lines file
    
            gameState <- readMVar gs
            let rendered = unlines $ take 13 ls ++ renderBoard (board gameState) ++ drop 13 ls where
            
            respond $ responseLBS status200 [("Content-Type", "text/html")] $ BLU.fromString rendered

main :: IO ()
main = do
    gameState <- newMVar $ GameState (replicate 9 Nothing) X
    run 80 $ app gameState
