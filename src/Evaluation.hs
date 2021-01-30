module Evaluation where

import Board
import qualified Data.Vector as V
import Data.List

invertIndex :: Int -> Int
invertIndex i = 63 - i

{-# INLINE atIndexes #-}
atIndexes :: V.Vector Int -> [Int] -> [Int]
atIndexes v idxs = map (v V.!) idxs

{-# INLINE computeSquare #-}
computeSquare :: (V.Vector Int, V.Vector Int) -> ([Int], [Int]) -> Int
computeSquare (whiteTable, blackTable) (whitePieces, blackPieces) = (foldr (+) 0 (whiteTable `atIndexes` whitePieces) - foldr (+) 0 (blackTable `atIndexes` blackPieces))

evaluateBoard :: Board -> Int
evaluateBoard board =
    material + pawnsq + knightsq + bishopsq + rooksq + queensq + kingsq
  where
    material = 100*(length wp - length bp)
              +320*(length wn - length bn)
              +330*(length wb - length bb)
              +500*(length wr - length br)
              +900*(length wq - length bq)
              +20000*(length wk - length bk)

    pawnsq = computeSquare (whitePawnsTable, blackPawnsTable) (wp, bp)
    knightsq = computeSquare (whiteKnightsTable, blackKnightsTable) (wn, bn)
    bishopsq = computeSquare (whiteBishopsTable, blackBishopsTable) (wb, bb)
    rooksq = computeSquare (whiteRooksTable, blackRooksTable) (wr, br)
    queensq = computeSquare (whiteQueensTable, blackQueensTable) (wq, bq)
    kingsq = computeSquare (if isEndGame then (whiteKingsEndGameTable, blackKingsEndGameTable) else (whiteKingsTable, blackKingsTable)) (wk, bk)

    isEndGame = case (null wq, null bq) of
      (True, True) -> True
      (True, _)    -> (sum $ map length [bn, bb, br]) <= 1
      (_, True)    -> (sum $ map length [wn, wb, wr]) <= 1
      (_,_)        -> False

    (wp, bp) = pieceTypeIndexes Pawn board
    (wn, bn) = pieceTypeIndexes Knight board
    (wb, bb) = pieceTypeIndexes Bishop board
    (wr, br) = pieceTypeIndexes Rook board
    (wq, bq) = pieceTypeIndexes Queen board
    (wk, bk) = pieceTypeIndexes King board

pawnsTable = V.fromList [
  0,  0,  0,  0,  0,  0,  0,  0,
  5, 10, 10,-20,-20, 10, 10,  5,
  5, -5,-10,  0,  0,-10, -5,  5,
  0,  0,  0, 20, 20,  0,  0,  0,
  5,  5, 10, 25, 25, 10,  5,  5,
  10, 10, 20, 30, 30, 20, 10, 10,
  50, 50, 50, 50, 50, 50, 50, 50,
  0,  0,  0,  0,  0,  0,  0,  0
  ]

whitePawnsTable = pawnsTable
blackPawnsTable = V.reverse pawnsTable

knightsTable = V.fromList [
  -50,-40,-30,-30,-30,-30,-40,-50,
  -40,-20,  0,  5,  5,  0,-20,-40,
  -30,  5, 10, 15, 15, 10,  5,-30,
  -30,  0, 15, 20, 20, 15,  0,-30,
  -30,  5, 15, 20, 20, 15,  5,-30,
  -30,  0, 10, 15, 15, 10,  0,-30,
  -40,-20,  0,  0,  0,  0,-20,-40,
  -50,-40,-30,-30,-30,-30,-40,-50
  ]

whiteKnightsTable = knightsTable
blackKnightsTable = V.reverse knightsTable

bishopsTable = V.fromList [
  -20,-10,-10,-10,-10,-10,-10,-20,
  -10,  5,  0,  0,  0,  0,  5,-10,
  -10, 10, 10, 10, 10, 10, 10,-10,
  -10,  0, 10, 10, 10, 10,  0,-10,
  -10,  5,  5, 10, 10,  5,  5,-10,
  -10,  0,  5, 10, 10,  5,  0,-10,
  -10,  0,  0,  0,  0,  0,  0,-10,
  -20,-10,-10,-10,-10,-10,-10,-20
  ]

whiteBishopsTable = bishopsTable
blackBishopsTable = V.reverse bishopsTable

rooksTable = V.fromList [
  0,  0,  0,  5,  5,  0,  0,  0,
  -5,  0,  0,  0,  0,  0,  0, -5,
  -5,  0,  0,  0,  0,  0,  0, -5,
  -5,  0,  0,  0,  0,  0,  0, -5,
  -5,  0,  0,  0,  0,  0,  0, -5,
  -5,  0,  0,  0,  0,  0,  0, -5,
  5, 10, 10, 10, 10, 10, 10,  5,
  0,  0,  0,  0,  0,  0,  0,  0
  ]

whiteRooksTable = rooksTable
blackRooksTable = V.reverse rooksTable

queensTable = V.fromList [
  -20,-10,-10, -5, -5,-10,-10,-20,
  -10,  0,  0,  0,  0,  0,  0,-10,
  -10,  5,  5,  5,  5,  5,  0,-10,
  0,  0,  5,  5,  5,  5,  0, -5,
  -5,  0,  5,  5,  5,  5,  0, -5,
  -10,  0,  5,  5,  5,  5,  0,-10,
  -10,  0,  0,  0,  0,  0,  0,-10,
  -20,-10,-10, -5, -5,-10,-10,-20
  ]

whiteQueensTable = queensTable
blackQueensTable = V.reverse queensTable

kingsTable = V.fromList [
  20, 30, 10,  0,  0, 10, 30, 20,
  20, 20,  0,  0,  0,  0, 20, 20,
  -10,-20,-20,-20,-20,-20,-20,-10,
  -20,-30,-30,-40,-40,-30,-30,-20,
  -30,-40,-40,-50,-50,-40,-40,-30,
  -30,-40,-40,-50,-50,-40,-40,-30,
  -30,-40,-40,-50,-50,-40,-40,-30,
  -30,-40,-40,-50,-50,-40,-40,-30
  ]

whiteKingsTable = kingsTable
blackKingsTable = V.reverse kingsTable

kingsEndGameTable = V.fromList [
  -50,-40,-30,-20,-20,-30,-40,-50,
  -30,-20,-10,  0,  0,-10,-20,-30,
  -30,-10, 20, 30, 30, 20,-10,-30,
  -30,-10, 30, 40, 40, 30,-10,-30,
  -30,-10, 30, 40, 40, 30,-10,-30,
  -30,-10, 20, 30, 30, 20,-10,-30,
  -30,-30,  0,  0,  0,  0,-30,-30,
  -50,-30,-30,-30,-30,-30,-30,-50
  ]

whiteKingsEndGameTable = kingsEndGameTable
blackKingsEndGameTable = V.reverse kingsEndGameTable