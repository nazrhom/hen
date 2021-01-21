import Test.HUnit

import Fen
import Board
import GameTree

depth :: Int
depth = 5

main :: IO ()
main = do
  runTestTT tests
  return ()

-- Siegbert Tarrasch vs. Max Kurschner
-- 1. Qg6+ hxg6 2. Bxg6#
mateInTwo = TestCase (assertEqual "1+2=3" move (Move (F,5) (G,6)))
  where
    board = fromFEN "r2qk2r/pb4pp/1n2Pb2/2B2Q2/p1p5/2P5/2B2PPP/RN2R1K1 w - - 1 0"
    (gs, _) = negamax board (active board) depth
    Just move = lastMove gs

tests = TestList [TestLabel "mateInTwo" mateInTwo]