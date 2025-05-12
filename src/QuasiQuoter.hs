{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module QuasiQuoter (chess) where

import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH
import Board
    ( Position,
      Column(H, A),
      Piece(..),
      Colour(..),
      PieceType(..),
      BoardVec (BoardVec) )
import Data.List (dropWhileEnd, foldl')
import Data.Char (isSpace)
import qualified Data.Vector as V
import Control.Monad.State (runStateT, StateT, execStateT, evalStateT, MonadState (put, get))
import Fen (invertLines)


chess :: QuasiQuoter
chess = QuasiQuoter
  chessExp
  (const $ fail "Cannot use chess in a pattern context")
  (const $ fail "Cannot use chess in a type context")
  (const $ fail "Cannot use chess in a declaration context")

chessExp :: String -> Q Exp
chessExp input = do
  board <- invertLines <$> parseAscii (stripWhitespace input)
  pure . AppE boardVecExp $ AppE fromListExp $ pieceListExp board
  where
    stripWhitespace = filter (not . isSpace)

    fromListExp :: Exp
    fromListExp = VarE 'V.fromList

    boardVecExp :: Exp
    boardVecExp = ConE 'BoardVec

    nothingExp :: Exp
    nothingExp = ConE 'Nothing

    justPieceExp :: Piece -> Exp
    justPieceExp (Piece c ty) = AppE (ConE 'Just) $ AppE (AppE (ConE 'Piece) (colExp c)) $ pieceTyExp ty

    colExp :: Colour -> Exp
    colExp White = ConE 'White
    colExp Black = ConE 'Black

    pieceTyExp :: PieceType -> Exp
    pieceTyExp King = ConE 'King
    pieceTyExp Queen = ConE 'Queen
    pieceTyExp Pawn = ConE 'Pawn
    pieceTyExp Knight = ConE 'Knight
    pieceTyExp Rook = ConE 'Rook
    pieceTyExp Bishop = ConE 'Bishop

    pieceListExp :: [Maybe Piece] -> Exp
    pieceListExp = ListE . fmap (maybe nothingExp justPieceExp)


parseAscii :: MonadFail m => String -> m [Maybe Piece]
parseAscii s = pure $ map parseChar s
  where
    parseChar :: Char -> Maybe Piece
    parseChar c = case fromEnum c of
      9812 -> Just $ Piece White King
      9813 -> Just $ Piece White Queen
      9814 -> Just $ Piece White Rook
      9815 -> Just $ Piece White Bishop
      9816 -> Just $ Piece White Knight
      9817 -> Just $ Piece White Pawn
      9818 -> Just $ Piece Black King
      9819 -> Just $ Piece Black Queen
      9820 -> Just $ Piece Black Rook
      9821 -> Just $ Piece Black Bishop
      9822 -> Just $ Piece Black Knight
      9823 -> Just $ Piece Black Pawn
      _ -> Nothing


test :: String
test = "♜♞♝♛♚♝♞♜ \
       \♟♟♟♟♟♟♟♟ \
       \□■□■□■□■ \
       \■□■□■□■□ \
       \□■□■♙■□■ \
       \■□■□■□■□ \
       \♙♙♙♙□♙♙♙ \ 
       \♖♘♗♕♔♗♘♖"
