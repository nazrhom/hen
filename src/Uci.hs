module Uci where

import Board
import Fen
import Text.Parsec
import Data.Char (toLower)

data Command = UCI | QUIT | READY | NEWGAME | GO | FEN GameState | STARTPOS [Move] deriving Show

parseUCICommand :: String -> Either ParseError Command
parseUCICommand = parse parseCommand ""

parseCommand :: Parsec String () Command
parseCommand = try parseNewGame <|> parseUCI <|> parseQuit <|> parseReady <|> parsePosition <|> parseGo

parseUCI, parseQuit, parseReady, parseNewGame, parsePosition, parseFenPos, parseStartingPos, parseGo :: Parsec String () Command

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

parseGo = do
  string "go"
  many anyChar
  return GO

parseUCIMoves :: Parsec String () [Move]
parseUCIMoves = moves <|> none
  where
    moves = do
      space
      string "moves"
      space
      sepBy1 parseUCIMove space

    none = return []

toUCI :: Move -> String
toUCI (Move (sCol, sRow) (dCol, dRow)) = (map toLower $ show sCol) ++ (show sRow) ++ (map toLower $ show dCol) ++ (show dRow)
toUCI (Castle White Short) = "e1g1"
toUCI (Castle White Long)  = "e1c1"
toUCI (Castle Black Short) = "e8g8"
toUCI (Castle Black Long)  = "e8c8"
toUCI (Promote (sCol, sRow) pty) = (map toLower $ show sCol) ++ show sRow ++ (map toLower $ show sCol) ++ show (sRow + 1) ++ show pty

parseUCIMove :: Parsec String () Move
parseUCIMove = do
  startCol <- letter
  startRow <- digit
  endCol <- letter
  endRow <- digit
  mbPromote <- (Just <$> try letter) <|> pure Nothing
  case mbPromote of
    Just pt -> return $ Promote (readColumn startCol, read [startRow]) (toPt pt)
    Nothing -> case ((readColumn startCol, read [startRow]), (readColumn endCol, read [endRow])) of
      ((E,1), (G,1)) -> return $ Castle White Short
      ((E,1), (C,1)) -> return $ Castle White Long
      ((E,8), (G,8)) -> return $ Castle Black Short
      ((E,8), (C,8)) -> return $ Castle Black Long
      (src, dst) -> return $ Move src dst
  where
    toPt 'q' = Queen
    toPt 'r' = Rook
    toPt 'n' = Knight
    toPt 'b' = Bishop
    toPt _   = error "Unknown piece type in parseUCIMove"