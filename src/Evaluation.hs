{-# LANGUAGE BangPatterns #-}
module Evaluation where

import Board
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.List

invertIndex :: Int -> Int
invertIndex i = 63 - i

{-# SCC atIndexes #-}
{-# INLINE atIndexes #-}
atIndexes :: V.Vector Int -> [Int] -> [Int]
atIndexes v idxs = map (v V.!) idxs

{-# SCC atIndexesMV #-}
{-# INLINE atIndexesMV #-}
atIndexesMV :: MV.IOVector Int -> [Int] -> IO [Int]
atIndexesMV v idxs = mapM (v `MV.unsafeRead`) idxs

data PieceTables = PieceTables {
  pawnsTables :: (MV.IOVector Int, MV.IOVector Int),
  knightsTables :: (MV.IOVector Int, MV.IOVector Int),
  rooksTables :: (MV.IOVector Int, MV.IOVector Int),
  bishopsTables :: (MV.IOVector Int, MV.IOVector Int),
  queensTables :: (MV.IOVector Int, MV.IOVector Int),
  kingsTables :: (MV.IOVector Int, MV.IOVector Int),
  kingsEndGameTables :: (MV.IOVector Int, MV.IOVector Int)
}

genPieceTables :: IO PieceTables
genPieceTables = do
  whitePawns <- genWhitePawnsTable
  blackPawns <- genBlackPawnsTable
  whiteRooks <- genWhiteRooksTable
  blackRooks <- genBlackRooksTable
  whiteKnights <- genWhiteKnightsTable
  blackKnights <- genBlackKnightsTable
  whiteBishops <- genWhiteBishopsTable
  blackBishops <- genBlackBishopsTable
  whiteQueens <- genWhiteQueensTable
  blackQueens <- genBlackQueensTable
  whiteKings <- genWhiteKingsTable
  blackKings <- genBlackKingsTable
  whiteKingsEndGame <- genWhiteKingsEndGameTable
  blackKingsEndGame <- genBlackKingsEndGameTable
  return $ PieceTables { 
    pawnsTables = (whitePawns, whitePawns),
    knightsTables = (whiteKnights, blackKnights),
    rooksTables = (whiteRooks, blackRooks),
    bishopsTables = (whiteBishops, blackBishops),
    queensTables = (whiteQueens, blackQueens),
    kingsTables = (whiteKings, blackKings),
    kingsEndGameTables = (whiteKingsEndGame, blackKingsEndGame)
    }
-- {-# INLINE computeSquare #-}
computeSquare :: (V.Vector Int, V.Vector Int) -> ([Int], [Int]) -> Int
computeSquare (whiteTable, blackTable) (whitePieces, blackPieces) = 
  (foldl' (+) 0 (whiteTable `atIndexes` whitePieces) - foldl' (+) 0 (blackTable `atIndexes` blackPieces))

computeSquareMV :: (MV.IOVector Int, MV.IOVector Int) -> ([Int], [Int]) -> IO Int
computeSquareMV (whiteTable, blackTable) (whitePieces, blackPieces) = do
  whites <- whiteTable `atIndexesMV` whitePieces
  blacks <- blackTable `atIndexesMV` blackPieces
  return (foldl' (+) 0 whites - foldl' (+) 0 blacks)

evaluateBoard :: PieceTables -> Board -> IO Int
evaluateBoard pt board = do
    pawnsq <- computeSquareMV (whitePawnsTable, blackPawnsTable) (wp, bp)
    knightsq <- computeSquareMV (whiteKnightsTable, blackKnightsTable) (wn, bn)
    bishopsq <- computeSquareMV (whiteBishopsTable, blackBishopsTable) (wb, bb)
    rooksq <- computeSquareMV (whiteRooksTable, blackRooksTable) (wr, br)
    queensq <- computeSquareMV (whiteQueensTable, blackQueensTable) (wq, bq)
    kingsq <- computeSquareMV (if isEndGame then (whiteKingsEndGameTable, blackKingsEndGameTable) else (whiteKingsTable, blackKingsTable)) (wk, bk)
    return $ material + pawnsq + knightsq + bishopsq + rooksq + queensq + kingsq
  where
    material = 100*(length wp - length bp)
              +320*(length wn - length bn)
              +330*(length wb - length bb)
              +500*(length wr - length br)
              +900*(length wq - length bq)
              +20000*(length wk - length bk)

    isEndGame = case (null wq, null bq) of
      (True, True) -> True
      (True, _)    -> (sum $ map length [bn, bb, br]) <= 1
      (_, True)    -> (sum $ map length [wn, wb, wr]) <= 1
      (_,_)        -> False

    (whitePawnsTable, blackPawnsTable) = pawnsTables pt
    (whiteKnightsTable, blackKnightsTable) = knightsTables pt
    (whiteQueensTable, blackQueensTable) = queensTables pt
    (whiteKingsTable, blackKingsTable) = kingsTables pt
    (whiteRooksTable, blackRooksTable) = rooksTables pt
    (whiteBishopsTable, blackBishopsTable) = bishopsTables pt
    (whiteKingsEndGameTable, blackKingsEndGameTable) = kingsEndGameTables pt
    (wp, bp) = pieceTypeIndexes Pawn board
    (wn, bn) = pieceTypeIndexes Knight board
    (wb, bb) = pieceTypeIndexes Bishop board
    (wr, br) = pieceTypeIndexes Rook board
    (wq, bq) = pieceTypeIndexes Queen board
    (wk, bk) = pieceTypeIndexes King board


pawnsTable = [
  0,  0,  0,  0,  0,  0,  0,  0,
  5, 10, 10,-20,-20, 10, 10,  5,
  5, -5,-10,  0,  0,-10, -5,  5,
  0,  0,  0, 20, 20,  0,  0,  0,
  5,  5, 10, 25, 25, 10,  5,  5,
  10, 10, 20, 30, 30, 20, 10, 10,
  50, 50, 50, 50, 50, 50, 50, 50,
  0,  0,  0,  0,  0,  0,  0,  0
  ]

genWhitePawnsTable :: IO (MV.IOVector Int)
genWhitePawnsTable = MV.generate (length pawnsTable) (pawnsTable !!)

genBlackPawnsTable :: IO (MV.IOVector Int)
genBlackPawnsTable = MV.generate (length pawnsTable) (reverse pawnsTable !!)

knightsTable = [
  -50,-40,-30,-30,-30,-30,-40,-50,
  -40,-20,  0,  5,  5,  0,-20,-40,
  -30,  5, 10, 15, 15, 10,  5,-30,
  -30,  0, 15, 20, 20, 15,  0,-30,
  -30,  5, 15, 20, 20, 15,  5,-30,
  -30,  0, 10, 15, 15, 10,  0,-30,
  -40,-20,  0,  0,  0,  0,-20,-40,
  -50,-40,-30,-30,-30,-30,-40,-50
  ]

genWhiteKnightsTable :: IO (MV.IOVector Int)
genWhiteKnightsTable = MV.generate (length knightsTable) (knightsTable !!)

genBlackKnightsTable :: IO (MV.IOVector Int)
genBlackKnightsTable = MV.generate (length knightsTable) (reverse knightsTable !!)

bishopsTable = [
  -20,-10,-10,-10,-10,-10,-10,-20,
  -10,  5,  0,  0,  0,  0,  5,-10,
  -10, 10, 10, 10, 10, 10, 10,-10,
  -10,  0, 10, 10, 10, 10,  0,-10,
  -10,  5,  5, 10, 10,  5,  5,-10,
  -10,  0,  5, 10, 10,  5,  0,-10,
  -10,  0,  0,  0,  0,  0,  0,-10,
  -20,-10,-10,-10,-10,-10,-10,-20
  ]

genWhiteBishopsTable :: IO (MV.IOVector Int)
genWhiteBishopsTable = MV.generate (length bishopsTable) (bishopsTable !!)

genBlackBishopsTable :: IO (MV.IOVector Int)
genBlackBishopsTable = MV.generate (length bishopsTable) (reverse bishopsTable !!)

rooksTable = [
  0,  0,  0,  5,  5,  0,  0,  0,
  -5,  0,  0,  0,  0,  0,  0, -5,
  -5,  0,  0,  0,  0,  0,  0, -5,
  -5,  0,  0,  0,  0,  0,  0, -5,
  -5,  0,  0,  0,  0,  0,  0, -5,
  -5,  0,  0,  0,  0,  0,  0, -5,
  5, 10, 10, 10, 10, 10, 10,  5,
  0,  0,  0,  0,  0,  0,  0,  0
  ]

genWhiteRooksTable :: IO (MV.IOVector Int)
genWhiteRooksTable = MV.generate (length rooksTable) (rooksTable !!)

genBlackRooksTable :: IO (MV.IOVector Int)
genBlackRooksTable = MV.generate (length rooksTable) (reverse rooksTable !!)

queensTable = [
  -20,-10,-10, -5, -5,-10,-10,-20,
  -10,  0,  0,  0,  0,  0,  0,-10,
  -10,  5,  5,  5,  5,  5,  0,-10,
    0,  0,  5,  5,  5,  5,  0, -5,
   -5,  0,  5,  5,  5,  5,  0, -5,
  -10,  0,  5,  5,  5,  5,  0,-10,
  -10,  0,  0,  0,  0,  0,  0,-10,
  -20,-10,-10, -5, -5,-10,-10,-20
  ]

genWhiteQueensTable :: IO (MV.IOVector Int)
genWhiteQueensTable = MV.generate (length queensTable) (queensTable !!)

genBlackQueensTable :: IO (MV.IOVector Int)
genBlackQueensTable = MV.generate (length queensTable) (reverse queensTable !!)

kingsTable = [
  20, 30, 10,  0,  0, 10, 30, 20,
  20, 20,  0,  0,  0,  0, 20, 20,
  -10,-20,-20,-20,-20,-20,-20,-10,
  -20,-30,-30,-40,-40,-30,-30,-20,
  -30,-40,-40,-50,-50,-40,-40,-30,
  -30,-40,-40,-50,-50,-40,-40,-30,
  -30,-40,-40,-50,-50,-40,-40,-30,
  -30,-40,-40,-50,-50,-40,-40,-30
  ]

genWhiteKingsTable :: IO (MV.IOVector Int)
genWhiteKingsTable = MV.generate (length kingsTable) (kingsTable !!)

genBlackKingsTable :: IO (MV.IOVector Int)
genBlackKingsTable = MV.generate (length kingsTable) (reverse kingsTable !!)

kingsEndGameTable = [
  -50,-40,-30,-20,-20,-30,-40,-50,
  -30,-20,-10,  0,  0,-10,-20,-30,
  -30,-10, 20, 30, 30, 20,-10,-30,
  -30,-10, 30, 40, 40, 30,-10,-30,
  -30,-10, 30, 40, 40, 30,-10,-30,
  -30,-10, 20, 30, 30, 20,-10,-30,
  -30,-30,  0,  0,  0,  0,-30,-30,
  -50,-30,-30,-30,-30,-30,-30,-50
  ]

genWhiteKingsEndGameTable :: IO (MV.IOVector Int)
genWhiteKingsEndGameTable = MV.generate (length kingsEndGameTable) (kingsEndGameTable !!)

genBlackKingsEndGameTable :: IO (MV.IOVector Int)
genBlackKingsEndGameTable = MV.generate (length kingsEndGameTable) (reverse kingsEndGameTable !!)