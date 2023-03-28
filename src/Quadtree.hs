{-# LANGUAGE TupleSections #-}
module Quadtree where

import Graphics.Gloss (Point)
import qualified Graphics.Gloss.Data.Point.Arithmetic as P ((+), (-))
import Data.List (nub)

--     |  
--  NW | NE
--  --- ---
--  SW | SE
--     |
data IntercardinalDirection = NE | SE | SW | NW deriving (Show, Eq)

-- Point quadtree data structure
data Quadtree a = Node { point :: (Point, a), ne, se, sw, nw :: Quadtree a }
                | Leaf Point a
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
node :: (Point, a) -> Quadtree a
node x = Node x Empty Empty Empty Empty

-- Inserting element into quadtree
insert :: Point -> a -> Quadtree a -> Quadtree a
insert p a Empty  = Leaf p a
insert p1 v1 l@(Leaf p2 v2) = if p1 /= p2 then insert p1 v1 $ node (p2,v2) else l
insert p1 v1 n@(Node (p2,_) ne' se' sw' nw')
  | dir == NE = n { ne = insert p1 v1 ne' }
  | dir == SE = n { se = insert p1 v1 se' }
  | dir == SW = n { sw = insert p1 v1 sw' }
  | otherwise = n { nw = insert p1 v1 nw' }
  where
    dir = intercardinalDirection p2 p1

-- Mapping intercardinal directions to quadtree quarters
quarter :: IntercardinalDirection -> Quadtree a -> Quadtree a
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
query :: Point -> Point -> Quadtree a -> [(Point, a)]
query _  _  Empty  = []
query p1 p2 (Leaf c v) = [(c,v) | (p1,p2) `contains` c]
query p1@(x1, y1) p2@(x2, y2) n = subPoints ++ [(c,v) | (p1,p2) `contains` c]
  where
    (p3, p4, (c,v)) = ((x2, y1), (x1, y2), point n)
    subPoints  = concatMap ((\q -> query p1 p2 $ q n) . quarter) directions
    directions = nub $ map (intercardinalDirection c) [p1,p2,p3,p4]

-- Get all points in tree
points :: Quadtree a -> [(Point, a)]
points Empty = []
points (Leaf v a) = [(v,a)]
points (Node v ne' se' sw' nw') = v : points ne' ++ points se' ++ points sw' ++ points nw'

-- Get all values of points within distance 1 for given point
getPointNeighbours :: (Point, a) -> Quadtree a -> (Point, a, [a])
getPointNeighbours (p1,v) = (p1,v,) 
  . map snd . filter ((/=p1) . fst) 
  . query (p1 P.- (1,1)) (p1 P.+ (1,1)) 

-- Get all points within distance 1 for every point in tree
getPointsNeighbours :: Quadtree a -> [(Point, a, [a])]
getPointsNeighbours q = map (`getPointNeighbours` q) $ points q