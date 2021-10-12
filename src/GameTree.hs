module GameTree where

import Board
import MoveGen
import Evaluation
import Data.Foldable

negamax :: PieceTables -> GameState -> Colour -> Int -> IO (GameState, Int)
negamax pt gs colour depth = abPrune gs colour depth (-(8^8)) (8^8)
  where 
    abPrune gs colour depth alpha beta | depth == 0 = do
                score <- evaluateBoard pt (board gs)
                return (gs, blackOrWhite * score)
              | otherwise = do
                (result, (score,_,_)) <- foldlM f (initialGameState, (-(8^8), alpha, beta)) $ applyAll gs (genMoves gs colour)
                return (result, score)
        where
          f :: (GameState, (Int, Int, Int)) -> GameState -> IO (GameState, (Int, Int, Int))
          f (cur, x@(bestVal, al, bt)) b = if al >= bt then return (cur, x)
            else do
              (b', val) <- abPrune b (flipColour colour) (depth - 1) (negate bt) (negate al)
              let a' = max al (negate val)
              if negate val > bestVal then return (b, (negate val, a', bt)) else return (cur, (bestVal, a', bt))
          blackOrWhite = if colour == White then 1 else -1

negamaxSimple :: PieceTables -> GameState -> Colour -> Int -> IO (GameState, Int)
negamaxSimple pt gs colour depth | depth == 0 = do
                                score <- evaluateBoard pt (board gs)
                                return (gs, blackOrWhite * score)
                              | otherwise = foldlM f (initialGameState, -(8^8)) $ applyAll gs (genMoves gs colour)
  where 
    blackOrWhite = if colour == White then 1 else -1
    f :: (GameState, Int) -> GameState -> IO (GameState, Int)
    f (cur, bestVal) b = do
      (b', val) <- negamaxSimple pt b (flipColour colour) (depth - 1)
      if negate val > bestVal then return (b, negate val) else return (cur, bestVal)
