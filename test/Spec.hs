import Test.HUnit

import Fen
import Board
import GameTree
import MoveGen
import Evaluation

import Data.List (sort)
import Control.Monad (unless)

depth :: Int
depth = 5

main :: IO ()
main = do
  runTestTT tests
  return ()

-- Siegbert Tarrasch vs. Max Kurschner
-- 1. Qg6+ hxg6 2. Bxg6#
mateInTwo = TestLabel "mate in two" $ TestCase (do
    pt <- genPieceTables
    let gs = fromFEN "r2qk2r/pb4pp/1n2Pb2/2B2Q2/p1p5/2P5/2B2PPP/RN2R1K1 w - - 1 0"
    (gs', _) <- negamax pt gs (active gs) depth
    let Just move = lastMove gs'
    assertEqual "" (Move (F,5) (G,6)) move)

rookMove1 = TestCase (assertSameMoves "" expected moves)
  where
    gs = fromFEN "8/8/8/8/8/8/8/3R4 w - - 0 1"
    moves = genAllRookMoves gs White
    expected = [
        Move (D,1) (D,2),Move (D,1) (D,3),Move (D,1) (D,4),Move (D,1) (D,5),Move (D,1) (D,6),Move (D,1) (D,7),Move (D,1) (D,8),
        Move (D,1) (E,1),Move (D,1) (F,1),Move (D,1) (G,1),Move (D,1) (H,1),Move (D,1) (C,1),Move (D,1) (B,1),Move (D,1) (A,1)
      ]

rookMove2 = TestCase (assertSameMoves "" expected moves)
  where
    gs = fromFEN "8/8/8/3q4/8/8/8/3R4 w - - 0 1"
    moves = genAllRookMoves gs White
    expected = [
        Move (D,1) (D,2),Move (D,1) (D,3),Move (D,1) (D,4),Move (D,1) (D,5),
        Move (D,1) (E,1),Move (D,1) (F,1),Move (D,1) (G,1),Move (D,1) (H,1),Move (D,1) (C,1),Move (D,1) (B,1),Move (D,1) (A,1)
      ]

rookMove3 = TestCase (assertSameMoves "" expected moves)
  where
    gs = fromFEN "8/8/8/8/8/8/P7/Rq6 w - - 0 1"
    moves = genAllRookMoves gs White
    expected = [ Move (A,1) (B,1) ]

queenMove1 = TestCase (assertSameMoves "" expected moves)
  where
    gs = fromFEN "8/8/8/8/3Q4/8/8/8 w - - 0 1"
    moves = genAllQueenMoves gs White
    expected = [
      Move (D,4) (D,5),Move (D,4) (D,6),Move (D,4) (D,7),Move (D,4) (D,8),Move (D,4) (D,3),Move (D,4) (D,2),Move (D,4) (D,1),
      Move (D,4) (E,4),Move (D,4) (F,4),Move (D,4) (G,4),Move (D,4) (H,4),Move (D,4) (C,4),Move (D,4) (B,4),Move (D,4) (A,4),
      Move (D,4) (E,5),Move (D,4) (F,6),Move (D,4) (G,7),Move (D,4) (H,8),Move (D,4) (C,3),Move (D,4) (B,2),Move (D,4) (A,1),
      Move (D,4) (C,5),Move (D,4) (B,6),Move (D,4) (A,7),Move (D,4) (E,3),Move (D,4) (F,2),Move (D,4) (G,1)]

queenMove2 = TestCase (assertSameMoves "" expected moves)
  where
    gs = fromFEN "8/P7/5p2/8/p2Q4/3P4/8/p7 w - - 0 1"
    moves = genAllQueenMoves gs White
    expected = [
      Move (D,4) (D,5),Move (D,4) (D,6),Move (D,4) (D,7),Move (D,4) (D,8),
      Move (D,4) (E,4),Move (D,4) (F,4),Move (D,4) (G,4),Move (D,4) (H,4),Move (D,4) (C,4),Move (D,4) (B,4),Move (D,4) (A,4),
      Move (D,4) (E,5),Move (D,4) (F,6),Move (D,4) (C,3),Move (D,4) (B,2),Move (D,4) (A,1),
      Move (D,4) (C,5),Move (D,4) (B,6),Move (D,4) (E,3),Move (D,4) (F,2),Move (D,4) (G,1)]

knightMove1 = TestCase (assertSameMoves "" expected moves)
  where
    gs = fromFEN "8/8/8/4p3/8/5N2/3P3P/N3P3 w - - 0 1"
    moves = genAllKnightMoves gs White
    expected = [
      Move (A,1) (B,3),Move (A,1) (C,2),
      Move (F,3) (D,4),Move (F,3) (E,5),Move (F,3) (G,5),Move (F,3) (H,4),Move (F,3) (G,1)]

pawnMove1 = TestCase (assertSameMoves "" expected moves)
  where
    gs = fromFEN "rnbqkbnr/pppp2p1/7p/4p3/2NP3N/1PP1Pp2/P4PPP/R1BQKB1R b KQkq - 9 10"
    moves = genAllPawnMoves gs White
    expected = [
      Move (A,2) (A,3), Move (A,2) (A,4),
      Move (B,3) (B,4),
      Move (D,4) (D,5), Move (D,4) (E,5),
      Move (E,3) (E,4),
      Move (G,2) (F,3), Move (G,2) (G,3), Move (G,2) (G,4),
      Move (H,2) (H,3)
      ]

initial = TestLabel "initial position" $ TestCase $ assertEqual "There are 20 initial valid moves" 20 (length moves)
  where
    gs = fromFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1"
    moves = genMoves gs White

movement = TestLabel "movement" (TestList [
    rooks,
    queens,
    knights,
    pawns,
    mixed
  ])

rooks = TestLabel "rooks" (TestList [
    rookMove1,
    rookMove2,
    rookMove3
  ])

queens = TestLabel "queens" (TestList [
    queenMove1,
    queenMove2
  ])

knights = TestLabel "knights" (TestList [
    knightMove1
  ])

pawns = TestLabel "pawns" (TestList [
    pawnMove1
  ])

mixed = TestLabel "mixed" (TestList [
    initial
  ])

puzzles = TestLabel "puzzles" (TestList [
    mateInTwo
  ])

tests = TestList [
    puzzles,
    movement
  ]

-- Reimplement to compare sorted lists but show them unsorted in error msg
assertSameMoves :: (Ord a, Show a, Eq a) => String -> [a] -> [a] -> Assertion
assertSameMoves preface expected actual = unless (sortedA == sortedE) (assertFailure msg)
  where
    sortedA = sort actual
    sortedE = sort expected
    msg = (if null preface then "" else preface ++ "\n") ++
      "expected: " ++ show sortedE ++ "\n but got: " ++ show sortedA