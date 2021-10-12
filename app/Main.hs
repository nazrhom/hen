module Main where

import Board
import MoveGen
import Evaluation
-- import Fen
import Uci
import GameTree
import System.IO

import Data.Maybe (fromJust)

main :: IO ()
main = do
  let depth = 5
  pt <- genPieceTables
  m <- negamax pt initialGameState White depth
  putStrLn $ show m
  -- processLine depth initialGameState

printAndLog :: String -> IO ()
printAndLog s = do
  logLine s
  hPutStrLn stdout s
  hFlush stdout

logLine :: String -> IO ()
logLine s = do
  hPutStrLn stderr $ ">>>" ++ s

processLine :: PieceTables -> Int -> GameState -> IO ()
processLine pt depth gs = do
  l <- getLine
  logLine l
  case parseUCICommand l of
    Right UCI -> do
      printAndLog "id name Hen"
      printAndLog "id author Giovanni Garufi"
      printAndLog "uciok"
      processLine pt depth gs
    Right QUIT -> return ()
    Right READY -> do
      printAndLog "readyok"
      processLine pt depth gs
    Right NEWGAME -> processLine pt depth gs
    Right (FEN gs') -> processLine pt depth gs'
    Right (STARTPOS moves) -> processLine pt depth (sequenceMoves initialGameState moves)
    Right GO -> do
      (gs', score) <- negamax pt gs (active gs) depth
      putStrLn $ show gs'
      logLine $ show gs'
      logLine $ show (lastMove gs')
      logLine $ show score
      printAndLog $ "bestmove " ++ (toUCI (fromJust $ lastMove gs'))
      processLine pt depth gs'
    Left _ -> do
      logLine $ "unkown command " ++ l
      processLine pt depth gs