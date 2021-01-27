module Main where

import Board
import MoveGen
import Fen
import Uci
import GameTree
import System.IO

import Data.Maybe (fromJust)

logFilePath = "./log.txt"

main :: IO ()
main = do
  let depth = 6
  processLine depth initialGameState

printAndLog :: String -> IO ()
printAndLog s = do
  logLine s
  hPutStrLn stdout s
  hFlush stdout

logLine :: String -> IO ()
logLine s = do
  hPutStrLn stderr $ ">>>" ++ s

processLine :: Int -> GameState -> IO ()
processLine depth gs = do
  l <- getLine
  logLine l
  case parseUCICommand l of
    Right UCI -> do
      printAndLog "id name Hen"
      printAndLog "id author Giovanni Garufi"
      printAndLog "uciok"
      processLine depth gs
    Right QUIT -> return ()
    Right READY -> do
      printAndLog "readyok"
      processLine depth gs
    Right NEWGAME -> processLine depth gs
    Right (FEN gs') -> processLine depth gs'
    Right (STARTPOS moves) -> processLine depth (sequenceMoves initialGameState moves)
    Right GO -> do
      let (gs', score) = negamax gs (active gs) depth
      putStrLn $ show gs'
      logLine $ show gs'
      logLine $ show (lastMove gs')
      logLine $ show score
      printAndLog $ "bestmove " ++ (toUCI (fromJust $ lastMove gs'))
      processLine depth gs'
    Left _ -> do
      logLine $ "unkown command " ++ l
      processLine depth gs