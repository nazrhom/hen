{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
module Board where

import qualified Data.Vector as V
import Control.Monad.State
import Data.Char (toUpper)
import qualified Data.Set as S

import Data.Word
import Text.Printf
import Data.Bits

-- Pieces
data PieceType = King | Queen | Rook | Bishop | Knight | Pawn deriving (Eq, Ord)
data Colour = White | Black deriving (Show, Eq, Ord)
data Piece = Piece Colour PieceType deriving Eq

-- Moves
data CastleType = Long | Short deriving (Eq, Show, Ord)
data Move = Move Position Position | Castle Colour CastleType | Promote Position PieceType deriving (Eq, Show, Ord)

-- Board
newtype BoardVec = BoardVec (V.Vector (Maybe Piece))

data Board = Board {
    whitePawns :: !Word64
  , blackPawns :: !Word64
  , whiteKnights :: !Word64
  , blackKnights :: !Word64
  , whiteBishops :: !Word64
  , blackBishops :: !Word64
  , whiteRooks :: !Word64
  , blackRooks :: !Word64
  , whiteQueens :: !Word64
  , blackQueens :: !Word64
  , whiteKings :: !Word64
  , blackKings :: !Word64
  , allWhitePieces :: !Word64
  , allBlackPieces :: !Word64
  , allPieces :: !Word64
}

indexesToBitBoard :: V.Vector Int -> Word64
indexesToBitBoard xs = go (V.toList xs) 0
  where
    go (x:xs) b = go xs $ b `setBit` x
    go []     b = b

fromVec :: BoardVec -> Board
fromVec board = Board {
        whitePawns = whitePawns
      , blackPawns = blackPawns
      , whiteKnights = whiteKnights
      , blackKnights = blackKnights
      , whiteBishops = whiteBishops
      , blackBishops = blackBishops
      , whiteRooks = whiteRooks
      , blackRooks = blackRooks
      , whiteQueens = whiteQueens
      , blackQueens = blackQueens
      , whiteKings = whiteKings
      , blackKings = blackKings
      , allWhitePieces = allWhitePieces
      , allBlackPieces = allBlackPieces
      , allPieces = allPieces
    }
  where
    whitePawns = indexesToBitBoard wp
    blackPawns = indexesToBitBoard bp
    whiteKnights = indexesToBitBoard wn
    blackKnights = indexesToBitBoard bn
    whiteBishops = indexesToBitBoard wb
    blackBishops = indexesToBitBoard bb
    whiteRooks = indexesToBitBoard wr
    blackRooks = indexesToBitBoard br
    whiteQueens = indexesToBitBoard wq
    blackQueens = indexesToBitBoard bq
    whiteKings = indexesToBitBoard wk
    blackKings = indexesToBitBoard bk
    allWhitePieces = whitePawns .|. whiteKnights .|. whiteBishops .|. whiteRooks .|. whiteQueens .|. whiteKings
    allBlackPieces = blackPawns .|. blackKnights .|. blackBishops .|. blackRooks .|. blackQueens .|. blackKings
    allPieces = allWhitePieces .|. allBlackPieces
    convertPieceTypeIndexes pt board = let (w, b) = pieceTypeIndexesVec pt board in (V.map vecIndex2BBIndex w, V.map vecIndex2BBIndex b)
    (wp, bp) = convertPieceTypeIndexes Pawn board
    (wn, bn) = convertPieceTypeIndexes Knight board
    (wb, bb) = convertPieceTypeIndexes Bishop board
    (wr, br) = convertPieceTypeIndexes Rook board
    (wq, bq) = convertPieceTypeIndexes Queen board
    (wk, bk) = convertPieceTypeIndexes King board

toVec :: Board -> BoardVec
toVec board = execState go (BoardVec $ V.replicate 64 Nothing)
  where
    go = do
      placePieces wp (Piece White Pawn)
      placePieces wn (Piece White Knight)
      placePieces wb (Piece White Bishop)
      placePieces wr (Piece White Rook)
      placePieces wq (Piece White Queen)
      placePieces wk (Piece White King)
      placePieces bp (Piece Black Pawn)
      placePieces bn (Piece Black Knight)
      placePieces br (Piece Black Rook)
      placePieces bb (Piece Black Bishop)
      placePieces bq (Piece Black Queen)
      placePieces bk (Piece Black King)

    (wp, bp) = pieceTypeIndexes Pawn board
    (wn, bn) = pieceTypeIndexes Knight board
    (wb, bb) = pieceTypeIndexes Bishop board
    (wr, br) = pieceTypeIndexes Rook board
    (wq, bq) = pieceTypeIndexes Queen board
    (wk, bk) = pieceTypeIndexes King board

    placePieces pi p = do
      mapM (uncurry placePiece) $ zip (repeat p) $ pi

    placePiece p i = do
      b <- get
      put $ placeVec p (indexToPosition i) b

type Row = Int
data Column = A | B | C | D | E | F | G | H deriving (Show, Eq, Ord)
type Position = (Column, Row)

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

data CastlingRight = CastlingRight Colour CastleType deriving (Eq, Show, Ord)

data GameState = GameState {
  board :: !Board,
  active :: !Colour,
  castling :: !(S.Set CastlingRight),
  enPassant :: !(Maybe Position),
  halfMoveClock :: !Int,
  fullMove :: !Int,
  lastMove :: !(Maybe Move)
}

positionToIndexVec :: Position -> Int
positionToIndexVec (c, r) = (r - 1) * 8 + toInt c

positionToIndex :: Position -> Int
positionToIndex (c, r) = (r * 8) - (toInt c + 1)

vecIndex2BBIndex :: Int -> Int
vecIndex2BBIndex = positionToIndex . indexToPositionVec

indexToPositionVec :: Int -> Position
indexToPositionVec i | i >= 0 && i <= 63 = (fromInt (i `rem` 8), (i `quot` 8) + 1)
                  | otherwise = error "no position"

indexToPosition :: Int -> Position
indexToPosition i | i >= 0 && i <= 63 = (fromInt (abs ((i `rem` 8) - 7)), (i `quot` 8) + 1)
                  | otherwise = error "no position"

placeVec :: Piece -> Position -> BoardVec -> BoardVec
placeVec piece pos (BoardVec board) = BoardVec $ board V.// [(positionToIndex pos, Just piece)]

place :: Piece -> Position -> Board -> Board
place piece pos bb = case piece of
  (Piece White Pawn)   -> bb { whitePawns=whitePawns bb `setBit` i    , allWhitePieces=allWhitePieces bb `setBit` i, allPieces=allPieces bb `setBit` i }
  (Piece White Knight) -> bb { whiteKnights=whiteKnights bb `setBit` i, allWhitePieces=allWhitePieces bb `setBit` i, allPieces=allPieces bb `setBit` i }
  (Piece White Bishop) -> bb { whiteBishops=whiteBishops bb `setBit` i, allWhitePieces=allWhitePieces bb `setBit` i, allPieces=allPieces bb `setBit` i }
  (Piece White Rook)   -> bb { whiteRooks=whiteRooks bb `setBit` i    , allWhitePieces=allWhitePieces bb `setBit` i, allPieces=allPieces bb `setBit` i }
  (Piece White Queen)  -> bb { whiteQueens=whiteQueens bb `setBit` i  , allWhitePieces=allWhitePieces bb `setBit` i, allPieces=allPieces bb `setBit` i }
  (Piece White King)   -> bb { whiteKings=whiteKings bb `setBit` i    , allWhitePieces=allWhitePieces bb `setBit` i, allPieces=allPieces bb `setBit` i }
  (Piece Black Pawn)   -> bb { blackPawns=blackPawns bb `setBit` i    , allBlackPieces=allBlackPieces bb `setBit` i, allPieces=allPieces bb `setBit` i }
  (Piece Black Knight) -> bb { blackKnights=blackKnights bb `setBit` i, allBlackPieces=allBlackPieces bb `setBit` i, allPieces=allPieces bb `setBit` i }
  (Piece Black Bishop) -> bb { blackBishops=blackBishops bb `setBit` i, allBlackPieces=allBlackPieces bb `setBit` i, allPieces=allPieces bb `setBit` i }
  (Piece Black Rook)   -> bb { blackRooks=blackRooks bb `setBit` i    , allBlackPieces=allBlackPieces bb `setBit` i, allPieces=allPieces bb `setBit` i }
  (Piece Black Queen)  -> bb { blackQueens=blackQueens bb `setBit` i  , allBlackPieces=allBlackPieces bb `setBit` i, allPieces=allPieces bb `setBit` i }
  (Piece Black King)   -> bb { blackKings=blackKings bb `setBit` i    , allBlackPieces=allBlackPieces bb `setBit` i, allPieces=allPieces bb `setBit` i }
  where
    i = positionToIndex pos

removeVec :: Position -> BoardVec -> BoardVec
removeVec pos (BoardVec board) = BoardVec $ board V.// [(positionToIndex pos, Nothing)]

remove :: Position -> Board -> Board
remove pos bb =
    if allPieces bb `testBit` i
      then if allWhitePieces bb `testBit` i then
        bb {
            whitePawns=whitePawns bb `clearBit` i
          , whiteKnights=whiteKnights bb `clearBit` i
          , whiteBishops=whiteBishops bb `clearBit` i
          , whiteRooks=whiteRooks bb `clearBit` i
          , whiteQueens=whiteQueens bb `clearBit` i
          , whiteKings=whiteKings bb `clearBit` i
          , allWhitePieces=allWhitePieces bb `clearBit` i
          , allPieces=allPieces bb `clearBit` i
        }
      else if allBlackPieces bb `testBit` i then
        bb {
            blackPawns=blackPawns bb `clearBit` i
          , blackKnights=blackKnights bb `clearBit` i
          , blackBishops=blackBishops bb `clearBit` i
          , blackRooks=blackRooks bb `clearBit` i
          , blackQueens=blackQueens bb `clearBit` i
          , blackKings=blackKings bb `clearBit` i
          , allBlackPieces=allBlackPieces bb `clearBit` i
          , allPieces=allPieces bb `clearBit` i
        }
      else error "remove"
    else bb
  where
    i = positionToIndex pos

move :: Position -> Position -> Board -> Board
move src dest board = case board `atPosition` src of 
  Nothing -> error $ "Trying to move from empty square\n"
      ++ show board
      ++ "\nsrc:" ++ show src
      ++ "\ndest:" ++ show dest
  Just piece -> remove src (place piece dest (remove dest board))

atPosition :: Board -> Position -> Maybe Piece
atPosition board pos = board `atIndex` (positionToIndex pos)

atIndexVec :: BoardVec -> Int -> Maybe Piece
atIndexVec (BoardVec board) idx = board V.! idx

atIndex :: Board -> Int -> Maybe Piece
atIndex bb i =
  if not (allPieces bb `testBit` i) then Nothing
  else
  if allWhitePieces bb `testBit` i
    then if whitePawns bb `testBit` i then Just $ Piece White Pawn
      else if whiteKnights bb `testBit` i then Just $ Piece White Knight
      else if whiteBishops bb `testBit` i then Just $ Piece White Bishop
      else if whiteRooks bb `testBit` i then Just $ Piece White Rook
      else if whiteQueens bb `testBit` i then Just $ Piece White Queen
      else if whiteKings bb `testBit` i then Just $ Piece White King
      else error "indexBB white"
  else if allBlackPieces bb `testBit` i
    then if blackPawns bb `testBit` i then Just $ Piece Black Pawn
      else if blackKnights bb `testBit` i then Just $ Piece Black Knight
      else if blackBishops bb `testBit` i then Just $ Piece Black Bishop
      else if blackRooks bb `testBit` i then Just $ Piece Black Rook
      else if blackQueens bb `testBit` i then Just $ Piece Black Queen
      else if blackKings bb `testBit` i then Just $ Piece Black King
      else error "indexBB black"
  else error "indexBB unknown"

emptyBoard :: Board
emptyBoard = fromVec $ BoardVec $ V.replicate 64 Nothing

pieceIndexesVec :: Piece -> BoardVec -> V.Vector Int
pieceIndexesVec p (BoardVec b) = V.elemIndices (Just p) b

{-# INLINE pieceIndexes #-}
pieceIndexes :: Piece -> Board -> [Int]
pieceIndexes p bb = case p of
  (Piece White Pawn)   -> toIndexVector $ whitePawns bb
  (Piece White Knight) -> toIndexVector $ whiteKnights bb
  (Piece White Bishop) -> toIndexVector $ whiteBishops bb
  (Piece White Rook)   -> toIndexVector $ whiteRooks bb
  (Piece White Queen)  -> toIndexVector $ whiteQueens bb
  (Piece White King)   -> toIndexVector $ whiteKings bb
  (Piece Black Pawn)   -> toIndexVector $ blackPawns bb
  (Piece Black Knight) -> toIndexVector $ blackKnights bb
  (Piece Black Bishop) -> toIndexVector $ blackBishops bb
  (Piece Black Rook)   -> toIndexVector $ blackRooks bb
  (Piece Black Queen)  -> toIndexVector $ blackQueens bb
  (Piece Black King)   -> toIndexVector $ blackKings bb

{-# INLINE toIndexVector #-}
toIndexVector :: Word64 -> [Int]
toIndexVector b = go b []
  where
    go b l = if zeros == 64 then l else go (b `clearBit` zeros) (zeros:l)
     where
        zeros = countTrailingZeros b

pieceTypeIndexesVec :: PieceType -> BoardVec -> (V.Vector Int, V.Vector Int)
pieceTypeIndexesVec p (BoardVec b) = (V.elemIndices (Just $ Piece White p) b, V.elemIndices (Just $ Piece Black p) b)

{-# INLINE pieceTypeIndexes #-}
pieceTypeIndexes :: PieceType -> Board -> ([Int], [Int])
pieceTypeIndexes !p !bb = (pieceIndexes (Piece White p) bb , pieceIndexes (Piece Black p) bb)

flipColour :: Colour -> Colour
flipColour White = Black
flipColour Black = White

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

instance Show BoardVec where
  show (BoardVec b) = go  b
    where
      go :: (V.Vector (Maybe Piece)) -> String
      go b = if V.null b then "" else 
        let (line, rest) = V.splitAt 8 b in go rest ++ (concat $ map showSquare (V.toList line)) ++ "\n"
      
      showSquare :: Maybe Piece -> String
      showSquare (Just p) = show p
      showSquare Nothing = "."

instance Show Board where
  show b = show $ toVec b

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

showBitBoard :: Word64 -> IO ()
showBitBoard b = mapM_ putStrLn $ reverse $ go 0
  where
    go :: Int -> [String]
    go 8 = []
    go i = (show8Bit $ shiftR line (8*i)):(go (i+1))
      where
        mask = shift 255 (8*i)
        line = b .&. mask

show8Bit :: Word64 -> String
show8Bit i = printf "%08b" i

fromBoard :: Board -> GameState
fromBoard b = initialGameState { board = b }