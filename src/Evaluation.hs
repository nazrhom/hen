module Evaluation where

import Board
import qualified Data.Vector as V

evaluateBoard :: Board -> Int
evaluateBoard board@(Board b) = 
    material + pawnsq + knightsq + bishopsq + rooksq + queensq + kingsq
  where
    material = 100*(length wp - length bp)
              +320*(length wn - length bn)
              +330*(length wb - length bb)
              +500*(length wr - length br)
              +900*(length wq - length bq)
              +20000*(length wk - length bk)
    
    invertIndex :: Int -> Int
    invertIndex i = 63 - i

    computeSquare :: V.Vector Int -> (V.Vector Int, V.Vector Int) -> Int
    computeSquare table (whitePieces, blackPieces) = (sum (table `atIndexes` whitePieces) - sum (table `atIndexes` V.map invertIndex blackPieces))

    pawnsq = computeSquare pawnsTable (wp, bp)
    knightsq = computeSquare knightsTable (wn, bn)
    bishopsq = computeSquare bishopsTable (wb, bb)
    rooksq = computeSquare rooksTable (wr, br)
    queensq = computeSquare queensTable (wq, bq)
    kingsq = computeSquare kingsTable (wk, bk)

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

