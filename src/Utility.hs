module Utility where

import Board (Board (..))
import Control.Lens (element, (&), (.~))
import Data.Char (toUpper)
import Moves
import Pieces

update :: Board -> Piece -> (Int, Int) -> Board
update (Board b) p (row, col) = Board (b & element row . element col .~ p)

movePiece :: Board -> (Int, Int) -> (Int, Int) -> Board
movePiece (Board b) (startR, startC) (endR, endC) = b''
  where
    validMoves = getMoves (Board b) (startR, startC)
    b' = update (Board b) (Piece {pieceT = Empty, color = None}) (startR, startC)
    b'' = update b' (b !! startR !! startC) (endR, endC)

strToCoord :: String -> Maybe (Int, Int)
strToCoord str
  | isValidCoordStr str = Just (read [str !! 1] - 1, charToInt $ toUpper (head str))
  | otherwise = Nothing
  where
    isValidCoordStr :: String -> Bool
    isValidCoordStr str = length str == 2 && (str !! 1) `elem` ['1' .. '8'] && toUpper (head str) `elem` ['A' .. 'H']

    charToInt :: Char -> Int
    charToInt c
      | c == 'A' = 7
      | c == 'B' = 6
      | c == 'C' = 5
      | c == 'D' = 4
      | c == 'E' = 3
      | c == 'F' = 2
      | c == 'G' = 1
      | c == 'H' = 0
      | otherwise = -1

coordToStr :: (Int, Int) -> String
coordToStr (x, y) = letter y : number x
  where
    letter y
      | y == 7 = 'A'
      | y == 6 = 'B'
      | y == 5 = 'C'
      | y == 4 = 'D'
      | y == 3 = 'E'
      | y == 2 = 'F'
      | y == 1 = 'G'
      | y == 0 = 'H'
      | otherwise = 'X'

    number x = show $ x + 1