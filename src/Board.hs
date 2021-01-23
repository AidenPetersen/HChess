module Board where

import Data.List
import Pieces

newtype Board = Board [[Piece]]

instance Show Board where
  show (Board b) =
    "   H  G  F  E  D  C  B  A\n"
      ++ unlines (zipWith (:) ['1' ..] strBoard)
    where
      strBoard :: [String]
      strBoard = map ((++) "  " . (intercalate "  " . map show)) b
      
data BoardStatus = Checkmate | Check | Stalemate | Normal deriving Eq