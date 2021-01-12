module Main where

import Board
import MoveGen
import Fen
import GameTree
import Text.Parsec
import System.IO

import Data.Maybe (fromJust)

logFilePath = "./log.txt"

main :: IO ()
main = do
  let depth = 5
  processLine depth initialGameState

printAndLog :: String -> IO ()
printAndLog s = do
  logLine s
  hPutStrLn stdout s
  hFlush stdout

logLine :: String -> IO ()
logLine s = do
  hPutStrLn stderr $ ">>>" ++ s

data Command = UCI | QUIT | READY | NEWGAME | GO | FEN GameState | STARTPOS [Move] deriving Show

processLine :: Int -> GameState -> IO ()
processLine depth gs = do
  l <- getLine
  logLine l
  case parse parseCommand "" l of
    Right UCI -> do
      printAndLog $ "id name Hen"
      printAndLog $ "id author Giovanni Garufi"
      printAndLog $ "uciok"
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


parseCommand :: Parsec String () Command
parseCommand = try parseNewGame <|> parseUCI <|> parseQuit <|> parseReady <|> parsePosition <|> parseGo

parseUCI, parseQuit, parseReady, parseNewGame, parsePosition, parseFenPos, parseGo :: Parsec String () Command

parseUCI = do
  string "uci"
  return UCI

parseQuit = do
  string "quit"
  return QUIT

parseReady = do
  string "isready"
  return READY

parseNewGame = do
  string "ucinewgame"
  return NEWGAME

parsePosition = do
  string "position"
  space
  parseFenPos <|> parseStartingPos

parseFenPos = do
  string "fen"
  space
  FEN <$> parseFEN

parseStartingPos = do
  string "startpos"
  STARTPOS <$> parseUCIMoves

parseUCIMoves :: Parsec String () [Move]
parseUCIMoves = moves <|> none
  where
    moves = do
      space
      string "moves"
      space
      sepBy1 parseUCIMove space

    none = return []

parseGo = do
  string "go"
  many anyChar
  return GO