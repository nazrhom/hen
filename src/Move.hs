module Move where

import Board
import qualified Data.Vector as V

data CastleType = Long | Short deriving Show
data Move = Move (Position, Position) | Castle Colour CastleType | Promote Position PieceType deriving Show

genMoves :: Board -> Colour -> [Move]
genMoves board col = 
     genAllPawnMoves board col 
  ++ genAllKnightMoves board col
  ++ genAllBishopMoves board col
  ++ genAllRookMoves board col
  ++ genAllQueenMoves board col
  ++ genAllKingMoves board col

applyAll :: Board -> [Move] -> [Board]
applyAll board ms = map (apply board) ms

apply :: Board -> Move -> Board
apply board (Move (src, dst)) = move src dst board
apply board (Castle White Short) = move (E,1) (G,1) $ move (H,1) (F,1) board
apply board (Castle White Long) = move (E,1) (C,1) $ move (A,1) (D,1) board
apply board (Castle Black Short) = move (E,8) (G,8) $ move (H,8) (F,8) board
apply board (Castle Black Long) = move (E,8) (C,8) $ move (A,8) (D,8) board
apply board (Promote pos pty) = remove pos $ place (Piece colour pty) (col, row+1) board
  where
    Just (Piece colour Pawn) = board `atPosition` pos
    (col, row) = pos

genAllPawnMoves :: Board -> Colour -> [Move]
genAllPawnMoves board col = concat $ V.toList $ V.map (genPawnMoves board col) pawns
  where
    pawns = pieceIndexes (Piece col Pawn) board

genPawnMoves :: Board -> Colour -> Int -> [Move]
genPawnMoves b c i = genPawnTakes b c i ++ genPawnForwardMoves b c i

genPawnForwardMoves :: Board -> Colour -> Int -> [Move]
genPawnForwardMoves b c i = if isInStartingPosition then singleStep ++ doubleStep else if isAboutToPromote then promotion else singleStep
  where
    (col, row) = indexToPosition i
    isInStartingPosition = (c == White && row == 2) || (c == Black && row == 7)
    isAboutToPromote = (c == White && row == 7) || (c == Black && row == 2)
    singleStepSquare = if c == White then (col, row+1) else (col, row-1)
    doubleStepSquare = if c == White then (col, row+2) else (col, row-2)
    inbounds i = i >= 0 && i <= 63
    promotion = map (Promote (indexToPosition i)) [Queen, Knight]
    singleStep = if inbounds (positionToIndex singleStepSquare) && isEmpty b (positionToIndex singleStepSquare) then [Move ((col, row), singleStepSquare)] else []
    doubleStep = if inbounds (positionToIndex doubleStepSquare) && isEmpty b (positionToIndex singleStepSquare) && isEmpty b (positionToIndex doubleStepSquare) then [Move ((col, row), doubleStepSquare)] else []

genPawnTakes :: Board -> Colour -> Int -> [Move]
genPawnTakes b c i = 
    if i `mod` 8 == 0
    then checkRightTake
    else 
      if i `mod` 8 == 7
      then checkLeftTake 
      else checkLeftTake ++ checkRightTake

  where
    checkRightTake = if (inbounds rightTake && containsOpponentPiece b c rightTake) then [Move (current, indexToPosition rightTake)] else []
    checkLeftTake = if (inbounds leftTake && containsOpponentPiece b c leftTake) then [Move (current, indexToPosition leftTake)] else []
    inbounds i = i >= 0 && i <= 63
    rightTake = if c == White then i + 9 else i - 7
    leftTake = if c == White then i + 7 else i - 9
    current = indexToPosition i

isEmpty :: Board -> Int -> Bool
isEmpty b i | i < 0 || i > 63 = error "debug: isEmpty out of bounds"
 | otherwise = case b `atIndex` i of
  Nothing -> True
  Just p -> False

containsOpponentPiece :: Board -> Colour -> Int -> Bool
containsOpponentPiece b c i | i < 0 || i > 63 = error "debug: containsOpponentPiece out of bounds"
       | otherwise = case b `atIndex` i of
  Nothing -> False
  Just (Piece c' p) -> c /= c'

genAllKnightMoves :: Board -> Colour -> [Move]
genAllKnightMoves board col = concat $ V.toList $ V.map (genKnightMoves board col) knights
  where knights = pieceIndexes (Piece col Knight) board

genKnightMoves :: Board -> Colour -> Int -> [Move]
genKnightMoves board colour i = map moveFromCurr $ filter canMove $ filter inbounds allMoves
  where
    (col,r) = indexToPosition i
    c = toInt col
    allMoves = [(c-1, r-2), (c-2, r-1)
              , (c-2, r+1), (c-1, r+2)
              , (c+1, r+2), (c+2, r+1)
              , (c+2, r-1), (c+1, r-2)]
    inbounds (a,b) = (a >= 0 && a <= 7) && (b >= 1 && b <= 8)
    moveFromCurr (a,b) = Move ((col,r), (fromInt a, b))
    canMove (a,b) = case board `atPosition` (fromInt a, b) of 
      Nothing -> True
      Just (Piece c p) -> c /= colour

genAllRookMoves :: Board -> Colour -> [Move]
genAllRookMoves board col = concat $ V.toList $ V.map (genRookMoves board col) rooks
  where rooks = pieceIndexes (Piece col Rook) board

genRookMoves :: Board -> Colour -> Int -> [Move]
genRookMoves board colour i = north ++ south ++ east ++ west
  where
    (row, col) = indexToPosition i
    north = rayToMoves (row,col) $ genNorthRay board colour (row,col)
    south = rayToMoves (row,col) $ genSouthRay board colour (row,col)
    east = rayToMoves (row,col) $ genEastRay board colour (row,col)
    west = rayToMoves (row,col) $ genWestRay board colour (row,col)

genAllQueenMoves :: Board -> Colour -> [Move]
genAllQueenMoves board col = concat $ V.toList $ V.map (genQueenMoves board col) queens
  where queens = pieceIndexes (Piece col Queen) board

genQueenMoves :: Board -> Colour -> Int -> [Move]
genQueenMoves board colour i = north ++ south ++ east ++ west ++ northeast ++ northwest ++ southeast ++ southwest
  where
    (row, col) = indexToPosition i
    north = rayToMoves (row,col) $ genNorthRay board colour (row,col)
    south = rayToMoves (row,col) $ genSouthRay board colour (row,col)
    east = rayToMoves (row,col) $ genEastRay board colour (row,col)
    west = rayToMoves (row,col) $ genWestRay board colour (row,col)
    northeast = rayToMoves (row,col) $ genNorthEastRay board colour (row,col)
    northwest = rayToMoves (row,col) $ genNorthWestRay board colour (row,col)
    southeast = rayToMoves (row,col) $ genSouthEastRay board colour (row,col)
    southwest = rayToMoves (row,col) $ genSouthWestRay board colour (row,col)

genAllBishopMoves :: Board -> Colour -> [Move]
genAllBishopMoves board col = concat $ V.toList $ V.map (genBishopMoves board col) bishops
  where bishops = pieceIndexes (Piece col Bishop) board

genBishopMoves :: Board -> Colour -> Int -> [Move]
genBishopMoves board colour i = northeast ++ northwest ++ southeast ++ southwest
  where
    (row, col) = indexToPosition i
    northeast = rayToMoves (row,col) $ genNorthEastRay board colour (row,col)
    northwest = rayToMoves (row,col) $ genNorthWestRay board colour (row,col)
    southeast = rayToMoves (row,col) $ genSouthEastRay board colour (row,col)
    southwest = rayToMoves (row,col) $ genSouthWestRay board colour (row,col)

genAllKingMoves :: Board -> Colour -> [Move]
genAllKingMoves board col = concat $ V.toList $ V.map (genKingMoves board col) kings
  where kings = pieceIndexes (Piece col King) board

genKingMoves :: Board -> Colour -> Int -> [Move]
genKingMoves board colour i = (map moveFromCurr $ filter canMove $ filter inbounds allMoves) ++ longCastle ++ shortCastle
  where
    (col,r) = indexToPosition i
    c = toInt col
    allMoves = [(c, r+1), (c+1, r+1)
              , (c+1, r), (c+1, r-1)
              , (c, r-1), (c-1, r-1)
              , (c-1, r), (c-1, r+1)]
    inbounds (a,b) = (a >= 0 && a <= 7) && (b >= 1 && b <= 8)
    moveFromCurr (a,b) = Move ((col,r), (fromInt a, b))

    canMove (a,b) = case board `atPosition` (fromInt a, b) of 
      Nothing -> True
      Just (Piece c p) -> c /= colour
    longCastle = if canCastle board colour Long i then [Castle colour Long] else []
    shortCastle = if canCastle board colour Short i then [Castle colour Short] else []

canCastle :: Board -> Colour -> CastleType -> Int -> Bool
canCastle board White cty i = i == 4 && case cty of 
    Long -> all (isEmpty board) [1,2,3] 
    Short -> all (isEmpty board) [5,6]  
canCastle board Black cty i = i == 60 && case cty of 
    Long -> all (isEmpty board) [57,58,59] 
    Short -> all (isEmpty board) [61,62]

rayToMoves :: Position -> [Position] -> [Move]
rayToMoves src dests = Move <$> [(src, dest) | dest <- dests]

genRay :: Board -> Colour -> Maybe Position -> (Position -> Maybe Position) -> [Position]
genRay b c (Just pos) fn = 
    if isEmpty b index
    then
      pos:(genRay b c (fn pos) fn)
    else
      if containsOpponentPiece b c index then [pos] else []
  where 
    index = positionToIndex pos
genRay b c Nothing fn = []

genNorthRay :: Board -> Colour -> Position -> [Position]
genNorthRay b c pos = genRay b c (f pos) f
  where 
    f (col, row) = if row + 1 <= 8 then Just (col, row + 1) else Nothing

genSouthRay :: Board -> Colour -> Position -> [Position]
genSouthRay b c pos = genRay b c (f pos) f
  where 
    f (col, row) = if row - 1 >= 1 then Just (col, row - 1) else Nothing

genEastRay :: Board -> Colour -> Position -> [Position]
genEastRay b c pos = genRay b c (f pos) f
  where 
    f (col, row) = if colInt + 1 <= 7 then Just (fromInt $ colInt + 1, row) else Nothing
      where
        colInt = toInt col

genWestRay :: Board -> Colour -> Position -> [Position]
genWestRay b c pos = genRay b c (f pos) f
  where 
    f (col, row) = if colInt - 1 >= 0 then Just (fromInt $ colInt - 1, row) else Nothing
      where
        colInt = toInt col

genNorthEastRay :: Board -> Colour -> Position -> [Position]
genNorthEastRay b c pos = genRay b c (f pos) f
  where 
    f (col, row) = if row + 1 <= 8 && colInt + 1 <= 7 then Just (fromInt $ colInt + 1, row + 1) else Nothing
      where
        colInt = toInt col

genNorthWestRay :: Board -> Colour -> Position -> [Position]
genNorthWestRay b c pos = genRay b c (f pos) f
  where 
    f (col, row) = if row + 1 <= 8 && colInt - 1 >= 0 then Just (fromInt $ colInt - 1, row + 1) else Nothing
      where
        colInt = toInt col

genSouthEastRay :: Board -> Colour -> Position -> [Position]
genSouthEastRay b c pos = genRay b c (f pos) f
  where 
    f (col, row) = if row - 1 >= 1 && colInt + 1 <= 7 then Just (fromInt $ colInt + 1, row - 1) else Nothing
      where
        colInt = toInt col

genSouthWestRay :: Board -> Colour -> Position -> [Position]
genSouthWestRay b c pos = genRay b c (f pos) f
  where 
    f (col, row) = if row - 1 >= 1 && colInt - 1 >= 0 then Just (fromInt $ colInt - 1, row - 1) else Nothing
      where
        colInt = toInt col
