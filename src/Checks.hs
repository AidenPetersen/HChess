module Checks  where

import Board
import Data.List (elemIndices)
import Moves
import Pieces
import Utility

-- Check if the enemy has any moves that can attack your king.
isCheck :: Color -> Board -> Bool
isCheck c (Board b) = any (`elem` enemyMoves) kingPos
  where
    enemyMoves :: [(Int, Int)]
    enemyMoves = concat [getMoves (Board b) (i, j) | i <- [0 .. 7], j <- [0 .. 7], color (b !! i !! j) == oppositeColor c]
    kingPos :: [(Int, Int)]
    kingPos = [(i, j) | i <- [0 .. 7], j <- [0 .. 7], b !! i !! j == Piece {pieceT = King, color = c}]

-- Brute force check all of your possible moves, if none get you out of check you are in checkmate.
isCheckmate :: Color -> Board -> Bool
isCheckmate c (Board b) = and possibleBoards
  where
    ourPieces :: [(Int, Int)]
    ourPieces = [(i, j) | i <- [0 .. 7], j <- [0 .. 7], color (b !! i !! j) == c]

    -- Gets all the possible boards from moving one piece.
    allOutcomes :: (Int, Int) -> [Board]
    allOutcomes piece = map (movePiece (Board b) piece) moves
      where
        moves = getMoves (Board b) piece

    -- A boolean of if each board is in check after a moves
    possibleBoards :: [Bool]
    possibleBoards = map (isCheck c) $ concatMap allOutcomes ourPieces

isStalemate :: Color -> Board -> Bool
isStalemate c (Board b) = null $ concat [getMoves (Board b) (i, j) | i <- [0 .. 7], j <- [0 .. 7], color (b !! i !! j) == c]

getBoardStatus :: Color -> Board -> BoardStatus
getBoardStatus b c
  | isStalemate b c = Stalemate
  | isCheck b c = if isCheckmate b c then Checkmate else Check
  | otherwise = Normal
