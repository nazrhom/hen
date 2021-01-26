module MoveGen where

import Board
import qualified Data.Vector as V
import Data.List (delete)
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Set as S
import qualified Data.IntMap.Strict as M

type KnightMap = M.IntMap [Position]

knightMap :: KnightMap
knightMap = foldl (\m k -> M.insert k (go k m) m) M.empty [0..63]
  where
    go k m = map (\(a,b) -> (fromInt a, b)) $ filter inbounds allMoves
      where
        (col, row) = indexToPosition k
        c = toInt col
        r = row
        allMoves = [(c-1, r-2), (c-2, r-1)
                  , (c-2, r+1), (c-1, r+2)
                  , (c+1, r+2), (c+2, r+1)
                  , (c+2, r-1), (c+1, r-2)]
        inbounds (a,b) = (a >= 0 && a <= 7) && (b >= 1 && b <= 8)

genMoves :: GameState -> Colour -> [Move]
genMoves gs col =
     genAllPawnMoves gs col
  ++ genAllKnightMoves gs col
  ++ genAllBishopMoves gs col
  ++ genAllRookMoves gs col
  ++ genAllQueenMoves gs col
  ++ genAllKingMoves gs col

applyAll :: GameState -> [Move] -> [GameState]
applyAll gs moves = map (apply gs) moves

apply :: GameState -> Move -> GameState
apply gs m@(Move src dst) = gs {
    board=move src dst (board gs)
  , castling=loosesCastlingRights (board gs) m (castling gs)
  , fullMove=(fullMove gs) + 1
  , active=flipColour (active gs)
  , lastMove=Just m
  }
apply gs m@(Castle White Short) = gs {
    board=move (E,1) (G,1) $ move (H,1) (F,1) (board gs)
  , castling=removeCastlingRightsFor (castling gs) White
  , fullMove=(fullMove gs) + 1
  , active=flipColour (active gs)
  , lastMove=Just m
  }
apply gs m@(Castle White Long) = gs {
    board=move (E,1) (C,1) $ move (A,1) (D,1) (board gs)
  , castling=removeCastlingRightsFor (castling gs) White
  , fullMove=(fullMove gs) + 1
  , active=flipColour (active gs)
  , lastMove=Just m
  }
apply gs m@(Castle Black Short) = gs {
    board=move (E,8) (G,8) $ move (H,8) (F,8) (board gs)
  , castling=removeCastlingRightsFor (castling gs) Black
  , fullMove=(fullMove gs) + 1
  , active=flipColour (active gs)
  , lastMove=Just m
  }
apply gs m@(Castle Black Long) = gs {
    board=move (E,8) (C,8) $ move (A,8) (D,8) (board gs)
  , castling=removeCastlingRightsFor (castling gs) Black
  , fullMove=(fullMove gs) + 1
  , active=flipColour (active gs)
  , lastMove=Just m
  }
apply gs m@(Promote pos pty) = gs {
    board=remove pos $ place (Piece colour pty) (col, row+1) (board gs) 
  , fullMove=(fullMove gs) + 1
  , active=flipColour (active gs)
  , lastMove=Just m
  }
  where
    Just (Piece colour Pawn) = (board gs) `atPosition` pos
    (col, row) = pos

sequenceMoves :: GameState -> [Move] -> GameState
sequenceMoves g (m:ms) = sequenceMoves (apply g m) ms
sequenceMoves g [] = g

removeCastlingRightsFor :: S.Set CastlingRight -> Colour -> S.Set CastlingRight
removeCastlingRightsFor crs col = S.filter (\(CastlingRight c ty) -> c /= col) crs

loosesCastlingRights :: Board -> Move -> S.Set CastlingRight -> S.Set CastlingRight
loosesCastlingRights b (Move src dst) activeRights | S.null activeRights = S.empty 
                                                   | otherwise           =
  case ty of
    King -> activeRights `removeCastlingRightsFor` colour
    Rook -> case (colour, col) of
      (White, A) -> S.delete (CastlingRight White Long)   activeRights
      (Black, A) -> S.delete (CastlingRight Black Long)   activeRights
      (White, H) -> S.delete (CastlingRight White Short)  activeRights
      (Black, H) -> S.delete (CastlingRight Black Short)  activeRights
      _ -> activeRights
    _ -> case dst of
      (A, 1) -> S.delete (CastlingRight White Long)   activeRights
      (A, 8) -> S.delete (CastlingRight Black Long)   activeRights
      (H, 1) -> S.delete (CastlingRight White Short)  activeRights
      (H, 8) -> S.delete (CastlingRight Black Short)  activeRights
      _      -> activeRights
  where
    Just (Piece colour ty) = b `atPosition` src
    (col, _) = src

genAllPawnMoves :: GameState -> Colour -> [Move]
genAllPawnMoves gs col = concat $ V.toList $ V.map (genPawnMoves b col) pawns
  where
    b = board gs
    pawns = pieceIndexes (Piece col Pawn) b

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
    singleStep = if inbounds (positionToIndex singleStepSquare) && isEmpty b (positionToIndex singleStepSquare) then [Move (col, row) singleStepSquare] else []
    doubleStep = if inbounds (positionToIndex doubleStepSquare) && isEmpty b (positionToIndex singleStepSquare) && isEmpty b (positionToIndex doubleStepSquare) then [Move (col, row) doubleStepSquare] else []

genPawnTakes :: Board -> Colour -> Int -> [Move]
genPawnTakes b c i = 
    if i `mod` 8 == 0
    then checkRightTake
    else 
      if i `mod` 8 == 7
      then checkLeftTake 
      else checkLeftTake ++ checkRightTake
  where
    checkRightTake = if (inbounds rightTake && containsOpponentPiece b c rightTake) then [Move current $ indexToPosition rightTake] else []
    checkLeftTake = if (inbounds leftTake && containsOpponentPiece b c leftTake) then [Move current $ indexToPosition leftTake] else []
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

knightMoves :: Board -> Colour -> Int -> [Position]
knightMoves board colour i = filter canMove $ fromJust $ M.lookup i knightMap
  where
    canMove pos = case board `atPosition` pos of
      Nothing -> True
      Just (Piece c p) -> c /= colour

rookRays :: Board -> Colour -> Position -> [[Position]]
rookRays board colour (col, row) = [north, south, east, west]
  where
    north = genNorthRay board colour (col,row)
    south = genSouthRay board colour (col,row)
    east  = genEastRay board colour (col,row)
    west  = genWestRay board colour (col,row)

bishopRays :: Board -> Colour -> Position -> [[Position]]
bishopRays board colour (col, row) = [northEast, northWest, southEast, southWest]
  where
    northEast = genNorthEastRay board colour (col,row)
    northWest = genNorthWestRay board colour (col,row)
    southEast = genSouthEastRay board colour (col,row)
    southWest = genSouthWestRay board colour (col,row)

genAllKnightMoves :: GameState -> Colour -> [Move]
genAllKnightMoves gs col = concat $ V.toList $ V.map (genKnightMoves b col) knights
  where
    b = board gs
    knights = pieceIndexes (Piece col Knight) b

genKnightMoves :: Board -> Colour -> Int -> [Move]
genKnightMoves board colour i = map moveFromCurr $ knightMoves board colour i
  where
    (col,row) = indexToPosition i
    moveFromCurr (a,b) = Move (col,row) (a, b)
 
genAllRookMoves :: GameState -> Colour -> [Move]
genAllRookMoves gs col = concat $ V.toList $ V.map (genRookMoves b col) rooks
  where 
    rooks = pieceIndexes (Piece col Rook) b
    b = board gs

genRookMoves :: Board -> Colour -> Int -> [Move]
genRookMoves board colour i = north ++ south ++ east ++ west
  where
    (col, row) = indexToPosition i
    [north, south, east, west] = fmap (rayToMoves (col,row)) $ rookRays board colour (col, row)

genAllQueenMoves :: GameState -> Colour -> [Move]
genAllQueenMoves gs col = concat $ V.toList $ V.map (genQueenMoves b col) queens
  where
    b = board gs
    queens = pieceIndexes (Piece col Queen) b


genQueenMoves :: Board -> Colour -> Int -> [Move]
genQueenMoves board colour i = north ++ south ++ east ++ west ++ northEast ++ northWest ++ southEast ++ southWest
  where
    (col, row) = indexToPosition i
    [north, south, east, west] = fmap (rayToMoves (col,row)) $ rookRays board colour (col, row)
    [northEast, northWest, southEast, southWest] = fmap (rayToMoves (col,row)) $ bishopRays board colour (col, row)

genAllBishopMoves :: GameState -> Colour -> [Move]
genAllBishopMoves gs col = concat $ V.toList $ V.map (genBishopMoves b col) bishops
  where
    b = board gs
    bishops = pieceIndexes (Piece col Bishop) b

genBishopMoves :: Board -> Colour -> Int -> [Move]
genBishopMoves board colour i = northEast ++ northWest ++ southEast ++ southWest
  where
    (col, row) = indexToPosition i
    [northEast, northWest, southEast, southWest] = fmap (rayToMoves (col,row)) $ bishopRays board colour (col, row)

genAllKingMoves :: GameState -> Colour -> [Move]
genAllKingMoves gs col = concat $ V.toList $ V.map (genKingMoves gs col) kings
  where
    kings = pieceIndexes (Piece col King) (board gs)

genKingMoves :: GameState -> Colour -> Int -> [Move]
genKingMoves gs colour i = (map moveFromCurr $ filter canMove $ filter inbounds allMoves) ++ longCastle ++ shortCastle
  where
    (col,r) = indexToPosition i
    c = toInt col
    allMoves = [(c, r+1), (c+1, r+1)
              , (c+1, r), (c+1, r-1)
              , (c, r-1), (c-1, r-1)
              , (c-1, r), (c-1, r+1)]
    inbounds (a,b) = (a >= 0 && a <= 7) && (b >= 1 && b <= 8)
    moveFromCurr (a,b) = Move (col,r) (fromInt a, b)

    canMove (a,b) = case (board gs) `atPosition` (fromInt a, b) of
      Nothing -> True
      Just (Piece c p) -> c /= colour
    longCastle = if canCastle gs colour Long then [Castle colour Long] else []
    shortCastle = if canCastle gs colour Short then [Castle colour Short] else []

canCastle :: GameState -> Colour -> CastleType -> Bool
canCastle gs White cty = case cty of
    Long -> CastlingRight White Long `elem` activeRights && all (isEmpty b) [1,2,3] && all null (map (attackers White (board gs)) [1,2,3])
    Short -> CastlingRight White Short `elem` activeRights && all (isEmpty b) [5,6] && all null (map (attackers White (board gs)) [5,6])
  where
    b = board gs
    activeRights = castling gs
canCastle gs Black cty = case cty of
    Long -> CastlingRight Black Long `elem` activeRights && all (isEmpty b) [57,58,59] && all null (map (attackers White (board gs)) [57,58,59])
    Short -> CastlingRight Black Short `elem` activeRights && all (isEmpty b) [61,62] && all null (map (attackers White (board gs)) [61,62])
  where
    b = board gs
    activeRights = castling gs

attackers :: Colour -> Board -> Int -> [Piece]
attackers colour board i = inLOS ++ knights
  where
    (col, row) = indexToPosition i
    inLOS = catMaybes $ fmap ((board `atPosition`) . last) $ straight ++ diag 
    straight = filter (/= []) $ rookRays board colour (col, row) 
    diag = filter (/= []) $ bishopRays board colour (col, row)
    knights = catMaybes $ filter (== (Just $ Piece (flipColour colour) Knight)) $ map (board `atPosition`) $ fromJust $ M.lookup i knightMap

rayToMoves :: Position -> [Position] -> [Move]
rayToMoves src dests = uncurry Move <$> [(src, dest) | dest <- dests]

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