import Test.Hspec

import Control.Concurrent.MVar (MVar, newMVar)

import qualified GameSpec
import qualified AppSpec

import Game

main :: IO ()
main = do
  gs <- newMVar $ GameState (replicate 9 Nothing) X
  hspec $ spec gs

spec :: MVar GameState -> Spec
spec gs = do
  describe "Game"  GameSpec.spec
  describe "App" $ AppSpec.spec gs
