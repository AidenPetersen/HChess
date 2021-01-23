module Run where

import Board
import Checks
import Control.Monad (when)
import Moves
import Pieces
import Utility

whiteTurnP1 :: Board -> IO ()
whiteTurnP1 b
  | status == Stalemate = do
    print b
    putStrLn "Stalemate!"
  | status == Checkmate = do
    print b
    putStrLn "Black Wins!"
  | status == Check = do
    putStrLn "White: You are in check!"
    print b
    putStrLn "White: Select Piece: "
    input <- getLine
    let pos = strToCoord input
    validate pos b
  | status == Normal = do
    print b
    putStrLn "White: Select Piece: "
    input <- getLine
    let pos = strToCoord input
    validate pos b
  where
    status = getBoardStatus White b
    validate :: Maybe (Int, Int) -> Board -> IO ()
    validate Nothing (Board b) = putStrLn "Invalid Coordinate." >> whiteTurnP1 (Board b)
    validate (Just (x, y)) (Board b)
      | color piece == White && (getMoves (Board b) (x, y) /= []) = whiteTurnP2 (Board b) (x, y)
      | color piece == None = putStrLn "Not a piece." >> whiteTurnP1 (Board b)
      | color piece == Black = putStrLn "Not your piece." >> whiteTurnP1 (Board b)
      | otherwise = putStrLn "Piece has no moves." >> whiteTurnP1 (Board b)
      where
        piece = b !! x !! y

whiteTurnP2 :: Board -> (Int, Int) -> IO ()
whiteTurnP2 (Board b) (x, y) =
  do
    putStrLn $ "White: Move " ++ show (pieceT piece) ++ " to: "
    input <- getLine
    if input == "back"
      then whiteTurnP1 (Board b)
      else
        if input == "ls"
          then do
            putStrLn ("Moves: " ++ show (coordToStr <$> getMoves (Board b) (x, y)))
            putStrLn $ "White: Move " ++ show (pieceT piece) ++ " to: "
            input' <- getLine
            validate (x, y) (strToCoord input') (Board b)
          else validate (x, y) (strToCoord input) (Board b)
  where
    piece = b !! x !! y

    validate :: (Int, Int) -> Maybe (Int, Int) -> Board -> IO ()
    validate pos Nothing b = putStrLn "Invalid coordinate." >> whiteTurnP2 b pos
    validate (v, w) (Just (x, y)) (Board b)
      | (x, y) `elem` getMoves (Board b) (v, w) && isCheck White (movePiece (Board b) (v, w) (x, y)) = putStrLn "Cannot move here, you will be in check!" >> whiteTurnP2 (Board b) (v, w)
      | (x, y) `elem` getMoves (Board b) (v, w) = do
        putStrLn $ "Moving " ++ show (pieceT piece) ++ " @ " ++ coordToStr (v, w) ++ " to " ++ coordToStr (x, y) ++ "."
        blackTurnP1 (movePiece (Board b) (v, w) (x, y))
      | otherwise = putStrLn ("Invalid move, can move to: " ++ show (coordToStr <$> getMoves (Board b) (v, w))) >> whiteTurnP2 (Board b) (v, w)
      where
        piece = b !! v !! w

blackTurnP1 :: Board -> IO ()
blackTurnP1 b
  | status == Stalemate = do
    print b
    putStrLn "Stalemate!"
  | status == Checkmate = do
    print b
    putStrLn "White Wins!"
  | status == Check = do
    putStrLn "Black: You are in check!"
    print b
    putStrLn "Black: Select Piece: "
    input <- getLine
    let pos = strToCoord input
    validate pos b
  | status == Normal = do
    print b
    putStrLn "Black: Select Piece: "
    input <- getLine
    let pos = strToCoord input
    validate pos b
  where
    status = getBoardStatus Black b
    validate :: Maybe (Int, Int) -> Board -> IO ()
    validate Nothing (Board b) = putStrLn "Invalid Coordinate." >> blackTurnP1 (Board b)
    validate (Just (x, y)) (Board b)
      | color piece == Black && not (null (getMoves (Board b) (x, y))) = blackTurnP2 (Board b) (x, y)
      | color piece == None = putStrLn "Not a piece." >> blackTurnP1 (Board b)
      | color piece == White = putStrLn "Not your piece." >> blackTurnP1 (Board b)
      | otherwise = putStrLn "Piece has no moves." >> blackTurnP1 (Board b)
      where
        piece = b !! x !! y

blackTurnP2 :: Board -> (Int, Int) -> IO ()
blackTurnP2 (Board b) (x, y) =
  do
    putStrLn $ "Black: Move " ++ show (pieceT piece) ++ " to: "
    input <- getLine
    if input == "back"
      then blackTurnP1 (Board b)
      else
        if input == "ls"
          then do
            putStrLn ("Moves: " ++ show (coordToStr <$> getMoves (Board b) (x, y)))
            putStrLn $ "Black: Move " ++ show (pieceT piece) ++ " to: "
            input' <- getLine
            validate (x, y) (strToCoord input') (Board b)
          else validate (x, y) (strToCoord input) (Board b)
  where
    piece = b !! x !! y

    validate :: (Int, Int) -> Maybe (Int, Int) -> Board -> IO ()
    validate pos Nothing b = putStrLn "Invalid coordinate" >> blackTurnP2 b pos
    validate (v, w) (Just (x, y)) (Board b)
      | (x, y) `elem` getMoves (Board b) (v, w)
          && isCheck Black (movePiece (Board b) (v, w) (x, y)) = do
        putStrLn "Cannot move here, you will be in check!"
        blackTurnP2 (Board b) (v, w)
      | (x, y) `elem` getMoves (Board b) (v, w) = do
        putStrLn $ "\nMoving  " ++ show (pieceT piece) ++ " @ " ++ coordToStr (v, w) ++ " to " ++ coordToStr (x, y)
        whiteTurnP1 (movePiece (Board b) (v, w) (x, y))
      | otherwise = putStrLn ("Invalid move, can move to: " ++ show (coordToStr <$> getMoves (Board b) (v, w))) >> blackTurnP2 (Board b) (v, w)
      where
        piece = b !! v !! w

startBoard :: Board
startBoard =
  Board
    [ [Piece {pieceT = Rook, color = White}, Piece {pieceT = Knight, color = White}, Piece {pieceT = Bishop, color = White}, Piece {pieceT = King, color = White}, Piece {pieceT = Queen, color = White}, Piece {pieceT = Bishop, color = White}, Piece {pieceT = Knight, color = White}, Piece {pieceT = Rook, color = White}],
      replicate 8 (Piece {pieceT = Pawn, color = White}),
      replicate 8 (Piece {pieceT = Empty, color = None}),
      replicate 8 (Piece {pieceT = Empty, color = None}),
      replicate 8 (Piece {pieceT = Empty, color = None}),
      replicate 8 (Piece {pieceT = Empty, color = None}),
      replicate 8 (Piece {pieceT = Pawn, color = Black}),
      [Piece {pieceT = Rook, color = Black}, Piece {pieceT = Knight, color = Black}, Piece {pieceT = Bishop, color = Black}, Piece {pieceT = King, color = Black}, Piece {pieceT = Queen, color = Black}, Piece {pieceT = Bishop, color = Black}, Piece {pieceT = Knight, color = Black}, Piece {pieceT = Rook, color = Black}]
    ]

run :: IO ()
run = do
  putStrLn
    "This is a simple game of chess. The turns are broken into 2 sections.\n\
    \First you select your piece by entering a coordinate such as 'a1'\n\
    \and then you choose where to move it. If you wish to go back after\n\
    \selecting your piece you can type 'back', and if you want to see \n\
    \the possible moves, you can type 'ls'."
  whiteTurnP1 startBoard
