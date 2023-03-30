
module Quadtree where
import Graphics.Gloss (Point)
import Control.Applicative (Applicative(liftA2))
import qualified Data.Set as Set
import Data.List (nub)

--     |  
--  NW | NE
--  --- ---
--  SW | SE
--     |
data IntercardinalDirection = NE | SE | SW | NW deriving (Show, Eq)

-- Point quadtree data structure
data Quadtree = Node { point :: Point, ne, se, sw, nw :: Quadtree }
              | Leaf Point
              | Empty
              deriving (Show)

-- intercardinal direction of (x2,y2) point relative to (x1,y1) point
intercardinalDirection :: Point -> Point -> IntercardinalDirection
intercardinalDirection (x1,y1) (x2,y2)
  | x1 <= x2 && y1 >  y2 = SE
  | x1 >  x2 && y1 >= y2 = SW
  | x1 >= x2 && y1 <  y2 = NW
  | otherwise = NE

-- Node with point
node :: Point -> Quadtree
node x = Node x Empty Empty Empty Empty

-- Inserting element into quadtree
insert :: Quadtree -> Point -> Quadtree
insert Empty p = Leaf p
insert l@(Leaf p2) p1 
  | p1 == p2  = l
  | otherwise = node p2 `insert` p1
insert n@(Node p2 ne' se' sw' nw') p1 
  | p1  == p2 = n 
  | dir == NE = n { ne = insert ne' p1  }
  | dir == SE = n { se = insert se' p1}
  | dir == SW = n { sw = insert sw' p1 }
  | otherwise = n { nw = insert nw' p1 }
  where
    dir = intercardinalDirection p2 p1

-- Mapping intercardinal directions to quadtree quarters
quarter :: IntercardinalDirection -> Quadtree -> Quadtree
quarter NE = ne
quarter SE = se
quarter SW = sw
quarter NW = nw

--  | ---- P2
--  |      |
--  |  P3  |
--  |      |
-- P1 ---- |
contains :: (Point, Point) -> Point  -> Bool
contains ((x1,y1), (x2,y2)) (x3,y3)
  = x1 <= x3 && x3 <= x2 &&
    y1 <= y3 && y3 <= y2

-- Get all points covered by a rectangular box
query :: Point -> Point -> Quadtree -> [Point]
query _  _  Empty  = []
query p1 p2 (Leaf p3) = [p3 | (p1,p2) `contains` p3]
query p1@(x1, y1) p2@(x2, y2) n = subPoints ++ [p | (p1,p2) `contains` p]
  where
    (p3, p4, p) = ((x2, y1), (x1, y2), point n)
    subPoints  = concatMap ((\q -> query p1 p2 $ q n) . quarter) directions
    directions = nub $ map (intercardinalDirection p) [p1,p2,p3,p4]

-- Get all points in tree
points :: Quadtree -> [Point]
points Empty = []
points (Leaf v) = [v]
points (Node v ne' se' sw' nw') = v : points ne' ++ points se' ++ points sw' ++ points nw'

-- neigbours :: (Num a, Num b, Enum a, Enum b, Eq a) => (a, b) -> [(a, b)]
neigbours :: (Num a, Num b, Enum a, Enum b, Eq a, Eq b) => (a, b) -> [(a, b)]
neigbours (x,y) = [(x',y') | x' <- [x-1..x+1], y' <- [y-1..y+1], (x',y') /= (x,y)]

aliveNeighbours :: Quadtree -> (Float, Float) -> Int
aliveNeighbours q (x,y) = length $ query (x-1,y-1) (x+1,y+1) q

-- nextGeneration :: Quadtree -> [(Float, Float)]
nextGeneration :: Quadtree -> Quadtree
nextGeneration q = foldl insert Empty (newAlive ++ newAlive')
  where 
    alive = points q
    dead = Set.fromList $ concatMap neigbours alive
    newAlive = filter (liftA2 (||) (==4) (==3) . aliveNeighbours q) alive
    newAlive' = filter ((==3) . aliveNeighbours q) $ Set.toList dead
