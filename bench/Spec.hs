import Criterion.Main

import Fen
import Board
import GameTree

main = defaultMain [
  bgroup "initial position" [
      bench "depth: 2 White" $ whnf (negamax initialGameState White) 2
    , bench "depth: 5 White" $ whnf (negamax initialGameState White) 5
    , bench "depth: 6 White" $ whnf (negamax initialGameState White) 6
    ]
  ]