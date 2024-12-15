module Day15 (part1, part2) where

import Data.List (foldl')
import Data.Map.Strict qualified as M
import Data.Tuple qualified as T
import Utils.Grid (Coord, Grid, makeGrid)

data BoxType = Single | L | R deriving (Show, Eq)

data Entity = Robot | Box BoxType | Wall | Empty deriving (Show, Eq)

data Move = N | E | S | W deriving (Show, Eq)

type Map = Grid Entity

parse :: String -> (Entity -> [Entity]) -> (Map, [Move])
parse input f = (makeGrid (map (concatMap parseEntity) es), map parseMove (concat $ drop 1 mv))
  where
    (es, mv) = break (== "") (lines input)
    parseMove = \case '^' -> N; 'v' -> S; '>' -> E; '<' -> W
    parseEntity = \case '#' -> f Wall; '.' -> f Empty; 'O' -> f (Box Single); '@' -> f Robot

swap :: Map -> Coord -> Coord -> Entity -> Map
swap m from to entity = M.adjust (const Empty) from (M.adjust (const entity) to m)

target :: Coord -> Move -> Coord
target (r, c) = \case N -> (r - 1, c); E -> (r, c + 1); S -> (r + 1, c); W -> (r, c - 1)

push :: Map -> Move -> Coord -> Maybe Map
push m move box = case boxes of
  [] -> Nothing
  boxes -> Just $ foldl' (\m' (bt, b) -> swap m' b (target b move) (Box bt)) m boxes
  where
    entity = m M.! box
    boxes = case (move, entity) of
      (_, Box Single) -> map (Single,) $ moveableBoxes m move box []
      (W, Box _) -> map T.swap $ (\xs -> zip xs (cycle [L, R])) $ moveableBoxes m move box []
      (E, Box _) -> map T.swap $ (\xs -> zip xs (cycle [R, L])) $ moveableBoxes m move box []
      (_, Box t) -> maybe [] (concatMap (\(l, r) -> [(L, l), (R, r)])) (moveableBigBoxes m move [completeBigBox box t] [])

completeBigBox :: Coord -> BoxType -> (Coord, Coord)
completeBigBox box = \case L -> (box, target box E); R -> (target box W, box)

moveableBoxes :: Map -> Move -> Coord -> [Coord] -> [Coord]
moveableBoxes m move box visited = case m M.! box' of
  Wall -> []
  Empty -> box : visited
  Box _ -> moveableBoxes m move box' (box : visited)
  where
    box' = target box move

moveableBigBoxes :: Map -> Move -> [(Coord, Coord)] -> [(Coord, Coord)] -> Maybe [(Coord, Coord)]
moveableBigBoxes m move boxes visited = case boxes' of
  Nothing -> Nothing
  Just [] -> Just (boxes ++ visited)
  Just boxes' -> moveableBigBoxes m move boxes' (boxes ++ visited)
  where
    boxes' = foldl' (\acc b -> (\xs -> (xs ++) <$> acc) =<< connected b) (Just []) boxes
    connected (l, r) = case (m M.! lbox', m M.! rbox') of
      (Wall, _) -> Nothing
      (_, Wall) -> Nothing
      (Empty, Empty) -> Just []
      (Empty, Box L) -> Just [completeBigBox rbox' L]
      (Box R, Empty) -> Just [completeBigBox lbox' R]
      (Box L, Box R) -> Just [completeBigBox lbox' L]
      (Box R, Box L) -> Just [completeBigBox lbox' R, completeBigBox rbox' L]
      (l, r) -> error (show l <> " " <> show r)
      where
        lbox' = target l move
        rbox' = target r move

step :: Map -> Coord -> [Move] -> Map
step m robot [] = m
step m robot (move : moves) = step m' robot' moves
  where
    (robot', m') =
      let robot' = target robot move
       in case m M.! robot' of
            Wall -> (robot, m)
            Empty -> (robot', swap m robot robot' Robot)
            Box _ -> case push m move robot' of
              Just m' -> (robot', swap m' robot robot' Robot)
              Nothing -> (robot, m)

entities :: Map -> Entity -> [Coord]
entities m entity = map fst $ M.toList $ M.filter (== entity) m

part1 :: String -> String
part1 input = show score
  where
    (m, moves) = parse input (: [])
    m' = step m (head $ entities m Robot) moves
    score = sum $ map (\(r, c) -> 100 * r + c) (entities m' (Box Single))

part2 :: String -> String
part2 input = show score
  where
    (m, moves) = parse input (\case Robot -> [Robot, Empty]; Box _ -> [Box L, Box R]; e -> [e, e])
    m' = step m (head $ entities m Robot) moves
    ((height, width), _) = M.findMax m
    boxes = zip (entities m' (Box L)) (entities m' (Box R))
    score = sum $ map (\((r, c), _) -> 100 * r + c) boxes
