module GameTree where

import Board
import Move
import Evaluation

negamax :: Board -> Colour -> Int -> (Board, Int)
negamax board colour depth = abPrune board colour depth (-(8^8)) (8^8)
  where 
    abPrune board colour depth alpha beta | depth == 0 = (board, blackOrWhite * evaluateBoard board)
              | otherwise = (\(result, (score,_,_)) -> (result,score)) $ foldl f (emptyBoard, (-(8^8), alpha, beta)) $ applyAll board (genMoves board colour)
        where
          f :: (Board, (Int, Int, Int)) -> Board -> (Board, (Int, Int, Int))
          f (cur, x@(bestVal, al, bt)) b = if al >= bt then (cur, x)
            else 
              let (b', val) = abPrune b (flipColour colour) (depth - 1) (negate bt) (negate al)
                  a' = max al (negate val)
              in if negate val > bestVal then (b', (negate val, a', bt)) else (cur, (bestVal, a', bt))
          blackOrWhite = if colour == White then 1 else -1

          flipColour White = Black
          flipColour Black = White

negamaxSimple :: Board -> Colour -> Int -> (Board, Int)
negamaxSimple board colour depth | depth == 0 = (board, blackOrWhite * evaluateBoard board)
                           | otherwise = foldl f (emptyBoard, -(8^8)) $ applyAll board (genMoves board colour)
  where 
    blackOrWhite = if colour == White then 1 else -1
    f :: (Board, Int) -> Board -> (Board, Int)
    f (cur, bestVal) b = 
      let (b', val) = negamaxSimple b (flipColour colour) (depth - 1)
      in if negate val > bestVal then (b', negate val) else (cur, bestVal)

    flipColour White = Black
    flipColour Black = White