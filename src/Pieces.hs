module Pieces where

data Color = Black | White | None deriving (Eq, Show)

data PieceType
  = Pawn
  | Rook
  | Knight
  | Bishop
  | Queen
  | King
  | Empty
  deriving (Eq, Show)

data Piece = Piece
  { pieceT :: PieceType,
    color :: Color
  } deriving(Eq)

instance Show Piece where
  show Piece {pieceT = Pawn, color = White} = "p"
  show Piece {pieceT = Queen, color = White} = "q"
  show Piece {pieceT = Rook, color = White} = "r"
  show Piece {pieceT = Knight, color = White} = "n"
  show Piece {pieceT = King, color = White} = "k"
  show Piece {pieceT = Bishop, color = White} = "b"
  show Piece {pieceT = Pawn, color = Black} = "P"
  show Piece {pieceT = Queen, color = Black} = "Q"
  show Piece {pieceT = Rook, color = Black} = "R"
  show Piece {pieceT = Knight, color = Black} = "N"
  show Piece {pieceT = King, color = Black} = "K"
  show Piece {pieceT = Bishop, color = Black} = "B"
  show _ = "_"

oppositeColor :: Color -> Color
oppositeColor White = Black
oppositeColor Black = White
oppositeColor None = None
