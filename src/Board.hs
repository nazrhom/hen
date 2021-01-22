{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
module Board where

import qualified Data.Vector as V
import Control.Monad.State
import Data.Char (toUpper)
import qualified Data.Set as S

data PieceType = King | Queen | Rook | Bishop | Knight | Pawn deriving Eq

data Colour = White | Black deriving (Show, Eq, Ord)

data Piece = Piece Colour PieceType deriving Eq
data Move = Move Position Position | Castle Colour CastleType | Promote Position PieceType deriving (Eq, Show)

newtype Board = Board (V.Vector (Maybe Piece))

type Position = (Column, Row)
data Column = A | B | C | D | E | F | G | H deriving (Show, Eq)

toInt :: Column -> Int
toInt A = 0
toInt B = 1
toInt C = 2
toInt D = 3
toInt E = 4
toInt F = 5
toInt G = 6
toInt H = 7

fromInt :: Int -> Column
fromInt i = case i of
  0 -> A
  1 -> B
  2 -> C
  3 -> D
  4 -> E
  5 -> F
  6 -> G
  7 -> H
  _ -> error "Column out of bounds"

type Row = Int

positionToIndex :: Position -> Int
positionToIndex (c, r) = (r - 1) * 8 + toInt c

indexToPosition :: Int -> Position
indexToPosition i | i >= 0 && i <= 63 = (fromInt (i `rem` 8), (i `quot` 8) + 1)
                  | otherwise = error "no position"

place :: Piece -> Position -> Board -> Board
place piece pos (Board board) = Board $ board V.// [(positionToIndex pos, Just piece)]

remove :: Position -> Board -> Board
remove pos (Board board) = Board $ board V.// [(positionToIndex pos, Nothing)]

move :: Position -> Position -> Board -> Board
move src dest board = case board `atPosition` src of 
  Nothing -> error $ "Trying to move from empty square\n"
      ++ show board
      ++ "\nsrc:" ++ show src
      ++ "\ndest:" ++ show dest
  Just piece -> remove src (place piece dest board) 

atPosition :: Board -> Position -> Maybe Piece
atPosition board pos = board `atIndex` (positionToIndex pos)

atIndex :: Board -> Int -> Maybe Piece
atIndex (Board board) idx = board V.! idx

atIndexes :: V.Vector a -> V.Vector Int -> V.Vector a
atIndexes v idxs = V.map (v V.!) idxs

emptyBoard :: Board
emptyBoard = Board $ V.replicate 64 Nothing

pieceIndexes :: Piece -> Board -> V.Vector Int
pieceIndexes p (Board b) = V.elemIndices (Just p) b

pieceTypeIndexes :: PieceType -> Board -> (V.Vector Int, V.Vector Int)
pieceTypeIndexes p (Board b) = (V.elemIndices (Just $ Piece White p) b, V.elemIndices (Just $ Piece Black p) b)

flipColour :: Colour -> Colour
flipColour White = Black
flipColour Black = White

data CastleType = Long | Short deriving (Eq, Show, Ord)
data CastlingRight = CastlingRight Colour CastleType deriving (Eq, Show, Ord)

data GameState = GameState {
  board :: Board,
  active :: Colour,
  castling :: S.Set CastlingRight,
  enPassant :: Maybe Position,
  halfMoveClock :: Int,
  fullMove :: Int,
  lastMove :: Maybe Move
}

instance Show PieceType where
  show King = "k"
  show Queen = "q"
  show Rook = "r"
  show Bishop = "b"
  show Knight = "n"
  show Pawn = "p"

instance Show Piece where
  show (Piece White p) = map toUpper (show p)
  show (Piece Black p) = show p

instance Show Board where
  show (Board b) = go  b
    where
      go :: (V.Vector (Maybe Piece)) -> String
      go b = if V.null b then "" else 
        let (line, rest) = V.splitAt 8 b in go rest ++ (concat $ map showSquare (V.toList line)) ++ "\n"
      
      showSquare :: Maybe Piece -> String
      showSquare (Just p) = show p
      showSquare Nothing = "."

initialBoard :: Board
initialBoard = execState placeInitialPieces emptyBoard
  where
    placeInitialPieces = do
      placePieces White
      mapM (uncurry placePiece) (zip (repeat (Piece White Pawn)) (zip row (repeat 2)))
      mapM (uncurry placePiece) (zip (repeat (Piece Black Pawn)) (zip row (repeat 7)))
      placePieces Black


    row = [A,B,C,D,E,F,G,H]
    pieceRow = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
    placePieces c = do
      mapM (uncurry placePiece) $ zip (map (Piece c) pieceRow) (zip row $ repeat (if c == White then 1 else 8))

    placePiece p pos = do
      b <- get
      put $ place p pos b

instance Show GameState where
  show g = show (board g)

printGameState :: GameState -> IO ()
printGameState gs = putStrLn $
     "\nactive: " ++ show a
  ++ "\ncastling:" ++ show c
  ++ "\nenPassant:" ++ show ep
  ++ "\nhalfMoveClock:" ++ show hmc
  ++ "\nfullMove:" ++ show fm
  ++ "\n"
  ++ show b
  where
    b = (board gs)
    a = (active gs)
    c = (castling gs)
    ep = (enPassant gs)
    hmc = (halfMoveClock gs)
    fm = (fullMove gs)

initialGameState :: GameState
initialGameState = GameState {
    board=initialBoard,
    active=White,
    castling=S.fromList [CastlingRight White Long, CastlingRight White Short, CastlingRight Black Long, CastlingRight Black Short],
    enPassant=Nothing,
    halfMoveClock=0,
    fullMove=0,
    lastMove=Nothing
  }