module UI (runGame) where 

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Logic 
import AI
import Data.Maybe(fromJust)


-- Window setup
window :: Display
window = InWindow "Tic Tac Toe" (600, 600) (100, 100)

background :: Color
background = white

fps :: Int
fps = 30

-- Start the game
runGame :: IO ()
runGame = play
  window
  background
  fps
  initial_state       -- from Logic.hs
  drawBoard
  handleEvent
  (\_ b -> b)         -- no time-based updates


--  Draw the current game board
drawBoard :: Board -> Picture
drawBoard board =
  Pictures [drawGrid, drawPieces board, statusText]
  where
    statusText
      | terminal board =
          translate (-150) (-250) $
          scale 0.3 0.3 $
          color red $
          Text (case winner board of
                  Empty  -> "Draw!"
                  p   -> show p ++ " wins!")
      | otherwise = Blank


--  Draws the 3x3 grid
drawGrid :: Picture
drawGrid = color black $
  Pictures [ line [(-150, 50), (150, 50)]
           , line [(-150, -50), (150, -50)]
           , line [(-50, 150), (-50, -150)]
           , line [(50, 150), (50, -150)]
           ]


--  Draws all pieces on the board
drawPieces :: Board -> Picture
drawPieces board = Pictures
  [ translate (fromIntegral (c * 100 - 100)) (fromIntegral (100 - r * 100)) (drawPiece p)
  | r <- [0..2], c <- [0..2], let p = getPiece board (r, c), p /= Empty ]


--  Draws X or O
drawPiece :: Piece -> Picture
drawPiece X = color blue  $ scale 0.3 0.3 $ Text "X"
drawPiece O = color red   $ scale 0.3 0.3 $ Text "O"
drawPiece _ = Blank


--  Convert mouse click coordinates to a board action
coordsToAction :: (Float, Float) -> Maybe Action
coordsToAction (x, y)
  | abs x > 150 || abs y > 150 = Nothing
  | otherwise = Just (row, col)
  where
    col = floor ((x + 150) / 100)
    row = 2 - floor ((y + 150) / 100)


--  Handle mouse clicks (player + AI)
handleEvent :: Event -> Board -> Board
handleEvent (EventKey (MouseButton LeftButton) Up _ (x, y)) board
  | terminal board = board  -- do nothing if game is over
  | otherwise =
      case coordsToAction (x, y) of
        Nothing -> board
        Just action ->
          case result board action of
            Nothing -> board
            Just newBoard ->
              if terminal newBoard
                then newBoard  -- human wins or draw
                else
                  case snd (minimax' newBoard) of
                    Nothing -> newBoard
                    Just aiMove ->
                      case result newBoard aiMove of
                        Just aiBoard -> aiBoard
                        Nothing      -> newBoard
handleEvent _ board = board


--  Retrieve piece from a coordinate (row, col)
getPiece :: Board -> (Int, Int) -> Piece
getPiece board (r, c) = (board !! r) !! c

