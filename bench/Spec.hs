import Criterion.Main

import Fen
import Board
import GameTree
import Evaluation

main = do
  pt <- genPieceTables
  defaultMain [
    bgroup "initial position" [
        bench "depth: 2 White" $ whnfIO (negamax pt initialGameState White 2)
      , bench "depth: 5 White" $ whnfIO (negamax pt initialGameState White 5)
      , bench "depth: 6 White" $ whnfIO (negamax pt initialGameState White 6)
      , bench "depth: 7 White" $ whnfIO (negamax pt initialGameState White 7)
      ]
    ]
