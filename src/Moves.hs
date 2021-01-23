module Moves (getMoves) where

import Board
import Data.List (delete, (\\))
import Pieces
  ( Color (Black, White),
    Piece (color, pieceT),
    PieceType (Bishop, Empty, King, Knight, Pawn, Queen, Rook),
    oppositeColor,
  )

getMoves :: Board -> (Int, Int) -> [(Int, Int)]
getMoves (Board b) (row, col)
  | pieceType == Queen = queenMoves (Board b) (row, col)
  | pieceType == Pawn = pawnMoves (Board b) (row, col)
  | pieceType == Rook = rookMoves (Board b) (row, col)
  | pieceType == King = kingMoves (Board b) (row, col)
  | pieceType == Bishop = bishopMoves (Board b) (row, col)
  | pieceType == Knight = knightMoves (Board b) (row, col)
  | otherwise = []
  where
    piece = b !! row !! col
    pieceType = pieceT $ b !! row !! col

queenMoves :: Board -> (Int, Int) -> [(Int, Int)]
queenMoves b (row, col) = concatMap (getRayMoves b (row, col)) directions
  where
    directions =
      [ (i, j)
        | i <- [-1 .. 1],
          j <- [-1 .. 1],
          not (j == 0 && i == 0)
      ] 

bishopMoves :: Board -> (Int, Int) -> [(Int, Int)]
bishopMoves b (row, col) = concatMap (getRayMoves b (row, col)) directions
  where
    directions =
      [ (i, j)
        | i <- [-1, 1],
          j <- [-1, 1]
      ]

rookMoves :: Board -> (Int, Int) -> [(Int, Int)]
rookMoves b (row, col) = concatMap (getRayMoves b (row, col)) directions
  where
    directions =
      [ (i, j)
        | i <- [-1 .. 1],
          j <- [-1 .. 1],
          i == 0 || j == 0,
          not (j == 0 && i == 0)
      ]

kingMoves :: Board -> (Int, Int) -> [(Int, Int)]
kingMoves (Board b) (row, col) = filter (\x -> color (b !! fst x !! snd x) /= c) uncheckedMoves
  where
    --list of moves before cheching for enemy pieces
    uncheckedMoves =
      [ (i + row, j + col)
        | i <- [-1 .. 1],
          j <- [-1 .. 1],
          not (j == 0 && i == 0),
          i + row `elem` [0 .. 7],
          j + col `elem` [0 .. 7]
      ]

    c = color $ b !! row !! col

knightMoves :: Board -> (Int, Int) -> [(Int, Int)]
knightMoves (Board b) (row, col) = filter (\x -> color (b !! fst x !! snd x) /= c) uncheckedMoves
  where
    -- A list of all of the possible moves before checking for friendly pieces
    uncheckedMoves =
      [ (i + row, j + col)
        | i <- [-2 .. 2],
          j <- [-2 .. 2],
          (i == -2 || i == 2) && (j == -1 || j == 1) || (j == -2 || j == 2) && (i == 1 || i == -1),
          i + row `elem` [0 .. 7],
          j + col `elem` [0 .. 7]
      ]
    c = color $ b !! row !! col

-- TODO: En Passant... probably never
pawnMoves :: Board -> (Int, Int) -> [(Int, Int)]
pawnMoves (Board b) (row, col)
  | c == White = whiteSingle ++ whiteDouble ++ whiteDiagonals
  | c == Black = blackSingle ++ blackDouble ++ blackDiagonals
  | otherwise = []
  where
    c = color $ b !! row !! col

    whiteSingle :: [(Int, Int)]
    whiteSingle
      | row `elem` [0 .. 6] && pieceT (b !! (row + 1) !! col) == Empty = [(row + 1, col)]
      | otherwise = []
    blackSingle
      | row `elem` [1 .. 7] && pieceT (b !! (row - 1) !! col) == Empty = [(row - 1, col)]
      | otherwise = []
    whiteDouble
      | row == 1 && pieceT (b !! (row + 2) !! col) == Empty = [(row + 2, col)]
      | otherwise = []
    blackDouble
      | row == 6 && pieceT (b !! (row - 2) !! col) == Empty = [(row - 2, col)]
      | otherwise = []
    whiteDiagonals = leftAttack ++ rightAttack
      where
        leftAttack
          | row `elem` [0 .. 6] && col `elem` [0 .. 6] && color (b !! (row + 1) !! (col + 1)) == oppositeColor c = [(row + 1, col + 1)]
          | otherwise = []
        rightAttack
          | row `elem` [0 .. 6] && col `elem` [1 .. 7] && color (b !! (row + 1) !! (col - 1)) == oppositeColor c = [(row + 1, col - 1)]
          | otherwise = []
    blackDiagonals = leftAttack ++ rightAttack
      where
        leftAttack
          | row `elem` [1 .. 7] && col `elem` [0 .. 6] && color (b !! (row - 1) !! (col + 1)) == oppositeColor c = [(row - 1, col + 1)]
          | otherwise = []
        rightAttack
          | row `elem` [1 .. 7] && col `elem` [1 .. 7] && color (b !! (row - 1) !! (col - 1)) == oppositeColor c = [(row - 1, col - 1)]
          | otherwise = []

-- Function to generalize Queen, Rook, and Bishop moves.
-- Takes 3 arguments, the board, position of the moving peice, and the direction it's moving.
getRayMoves :: Board -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
getRayMoves (Board b) (row, col) (ver, hor) = getRayMovesRec (Board b) c (row + ver, col + hor) [] (ver, hor)
  where
    currentPiece = b !! row !! col
    c = color $ b !! row !! col

    getRayMovesRec :: Board -> Color -> (Int, Int) -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
    getRayMovesRec (Board b) c (row, col) accum (ver, hor)
      | row `notElem` [0 .. 7] || col `notElem` [0 .. 7] || color nextPiece == c = accum -- Out of bounds or friendly returns ray
      | oppositeColor (color nextPiece) == c = (row, col) : accum -- Enemy Piece returns ray plus the enemy piece
      | otherwise = getRayMovesRec (Board b) c (row + ver, col + hor) ((row, col) : accum) (ver, hor) -- adds to accum and goes to next spot.
      where
        nextPiece = b !! row !! col
