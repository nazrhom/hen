module Fen where

import Board
import qualified Data.Vector as V
import qualified Data.Set as S
import Text.Parsec hiding (Column)
import Data.Char (toUpper, isUpper)
import Text.Read hiding (get)

readColumn :: Char -> Column
readColumn 'a' = A
readColumn 'b' = B
readColumn 'c' = C
readColumn 'd' = D
readColumn 'e' = E
readColumn 'f' = F
readColumn 'h' = H
readColumn 'g' = G

parseChar :: Parsec String () [Maybe Piece]
parseChar = do
  c <- letter <|> digit
  case readMaybe [c] :: Maybe Int of
    Just i -> return (replicate i Nothing)
    Nothing -> case toUpper c of
      'R' -> return $ [Just $ Piece (toColour c) Rook]
      'N' -> return $ [Just $ Piece (toColour c) Knight]
      'B' -> return $ [Just $ Piece (toColour c) Bishop]
      'Q' -> return $ [Just $ Piece (toColour c) Queen]
      'K' -> return $ [Just $ Piece (toColour c) King]
      'P' -> return $ [Just $ Piece (toColour c) Pawn]
      _   -> error "Unknown Piece"
  where
    toColour c = if isUpper c then White else Black

parseLine = sepBy1 (try parseChar) (char '/')
parseLines = many1 parseLine

parseColour = do
  c <- anyChar
  if (c == 'w') then return White
  else if (c == 'b') then return Black
  else error "Unknown colour"

parseEmpty = do
  char '-'
  return Nothing

parseCastling = S.fromList <$> (parseEmpty' <|> many1 parsePiece)
  where
    parsePiece = shortWhite <|> shortBlack <|> longBlack <|> longWhite
    parseEmpty' = do
      parseEmpty
      return []
    shortWhite = do
      char 'K'
      return $ CastlingRight White Short
    shortBlack = do
      char 'k'
      return $ CastlingRight Black Short
    longBlack = do
      char 'q'
      return $ CastlingRight Black Long
    longWhite = do
      char 'Q'
      return $ CastlingRight White Long

parseEnPassant :: Parsec String () (Maybe Position)
parseEnPassant = do
  c <- letter
  r <- digit
  return $ Just (readC c, read [r] :: Int)
  where
    readC c = readColumn c


parseFEN = do
  fen <- parseLines
  space
  colour <- parseColour
  space
  castling <- parseCastling
  space
  enPassant <- parseEnPassant <|> parseEmpty
  space
  halfM <- read <$> many1 digit
  space
  moves <- read <$> many1 digit
  return GameState 
    { 
      board = fromVec $ BoardVec $ V.fromList $ invertLines . concat . concat  $ fen,
      active = colour,
      castling = castling,
      enPassant = enPassant,
      halfMoveClock = halfM,
      fullMove = moves,
      lastMove = Nothing
    }

fromFEN s = case parse parseFEN "" s of
  Left e -> error (show e)
  Right p -> p

invertLines :: [a] -> [a]
invertLines xs@(a:as) = (invertLines rest) ++ x
  where 
    (x, rest) = splitAt 8 xs
invertLines [] = []
