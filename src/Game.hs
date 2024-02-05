module Game where

import Data.Maybe (isJust)

data Mark = X | O deriving (Eq, Show)

data Winner = Winner Mark | Draw

type Board = [Maybe Mark]

data GameState = GameState
  { board :: Board,
    activeMark :: Mark
  } deriving (Eq, Show)

renderBoard :: Board -> [String]
renderBoard b =
  map
    ( \(x, y) -> case x of
        Nothing -> "<a class=\"tile\" href=\"/mark/" ++ show y ++ "\"></a>\n"
        Just X -> "<a class=\"tile markX\" href=\"/mark/" ++ show y ++ "\">X</a>\n"
        Just O -> "<a class=\"tile markO\" href=\"/mark/" ++ show y ++ "\">O</a>\n"
    )
    $ zip b [0 ..]

makeMove :: GameState -> Int -> GameState
makeMove gs pos = GameState newBoard (nextMark mk)
  where
    b = board gs
    mk = activeMark gs
    newBoard =
      let (x, y : ys) = splitAt pos b
       in case y of
            Nothing -> x ++ Just mk : ys
            Just _ -> b
    nextMark X = O
    nextMark O = X

isWin :: Board -> Mark -> Bool
isWin b m = (any . all) (Just m ==) $ rows ++ cols ++ diag
  where
    rows = [[b !! (i + j) | j <- [0, 1, 2]] | i <- [0, 3, 6]]
    cols = [[b !! (i + j) | j <- [0, 3, 6]] | i <- [0, 1, 2]]
    diag = [[b !! 0, b !! 4, b !! 8], [b !! 2, b !! 4, b !! 6]]

checkWin :: Board -> Maybe Winner
checkWin b
  | isWin b X = Just (Winner X)
  | isWin b O = Just (Winner O)
  | all isJust b = Just Draw
  | otherwise = Nothing
