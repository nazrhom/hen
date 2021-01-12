module GameTree where

import Board
import MoveGen
import Evaluation

negamax :: GameState -> Colour -> Int -> (GameState, Int)
negamax gs colour depth = abPrune gs colour depth (-(8^8)) (8^8)
  where 
    abPrune gs colour depth alpha beta | depth == 0 = (gs, blackOrWhite * evaluateBoard (board gs))
              | otherwise = (\(result, (score,_,_)) -> (result,score)) $ foldl f (initialGameState, (-(8^8), alpha, beta)) $ applyAll gs (genMoves gs colour)
        where
          f :: (GameState, (Int, Int, Int)) -> GameState -> (GameState, (Int, Int, Int))
          f (cur, x@(bestVal, al, bt)) b = if al >= bt then (cur, x)
            else 
              let (b', val) = abPrune b (flipColour colour) (depth - 1) (negate bt) (negate al)
                  a' = max al (negate val)
              in if negate val > bestVal then (b, (negate val, a', bt)) else (cur, (bestVal, a', bt))
          blackOrWhite = if colour == White then 1 else -1

negamaxSimple :: GameState -> Colour -> Int -> (GameState, Int)
negamaxSimple gs colour depth | depth == 0 = (gs, blackOrWhite * evaluateBoard (board gs))
                           | otherwise = foldl f (initialGameState, -(8^8)) $ applyAll gs (genMoves gs colour)
  where 
    blackOrWhite = if colour == White then 1 else -1
    f :: (GameState, Int) -> GameState -> (GameState, Int)
    f (cur, bestVal) b = 
      let (b', val) = negamaxSimple b (flipColour colour) (depth - 1)
      in if negate val > bestVal then (b, negate val) else (cur, bestVal)
