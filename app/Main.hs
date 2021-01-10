module Main where

import Board
import GameTree

main :: IO ()
main = do
  putStrLn $ show $ negamax initialBoard White 5
