module GameState where

import Board
import Move

data CastlingRights = CastlingRights Colour CastleType deriving Show

data GameState = GameState {
  board :: Board,
  active :: Colour,
  castling :: Maybe [CastlingRights],
  enPassant :: Maybe Position,
  halfMoveClock :: Int,
  fullMove :: Int
}

instance Show GameState where
  show g = show (board g)


