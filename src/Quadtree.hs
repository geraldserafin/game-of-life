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

-- Point quadtree date structure
data Quadtree = Node { point :: Point, ne, se, sw, nw :: Quadtree }
              | Leaf (Maybe Point)
              deriving (Show)

-- intercardinal direction of (x2,y2) point relative to (x1,y1) point
intercardinalDirection :: Point -> Point -> IntercardinalDirection
intercardinalDirection (x1,y1) (x2,y2)
  | x1 <= x2 && y1 >  y2 = SE
  | x1 >  x2 && y1 >= y2 = SW
  | x1 >= x2 && y1 <  y2 = NW
  | otherwise = NE

-- Empty quadtree
leaf :: Quadtree
leaf = Leaf Nothing

-- Node with point
node :: Point -> Quadtree
node x = Node x leaf leaf leaf leaf

-- Inserting element into quadtree
insert :: Point -> Quadtree -> Quadtree
insert p (Leaf Nothing)  = Leaf (Just p)
insert p (Leaf (Just v)) = insert p $ node v
insert p n@(Node v ne' se' sw' nw')
  | dir == NE = n { ne = insert p ne' }
  | dir == SE = n { se = insert p se' }
  | dir == SW = n { sw = insert p sw' }
  | otherwise = n { nw = insert p nw' }
  where
    dir = intercardinalDirection v p

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
pointInBox :: Point -> Point -> Point -> Bool
pointInBox (x1,y1) (x2,y2) (x3,y3) 
  = x1 <= x3 && x3 <= x2 &&
    y1 <= y3 && y3 <= y2

-- Query all points covered by box
query :: Point -> Point -> Quadtree -> [Point]
query _  _  (Leaf Nothing)  = []
query p1 p2 (Leaf (Just v)) = [v | pointInBox p1 p2 v]
query p1@(x1, y1) p2@(x2, y2) n = subPoints ++ [v | pointInBox p1 p2 v]
  where
    (p3, p4, v) = ((x2, y1), (x1, y2), point n)
    subPoints   = concatMap ((\dir -> query p1 p2 $ dir n) . quarter) directions
    directions  = nub $ map (intercardinalDirection v) [p1,p2,p3,p4]

-- get all points in tree
points :: Quadtree -> [Point]
points (Leaf Nothing)  = []
points (Leaf (Just v)) = [v]
points (Node v ne' se' sw' nw') = v : points ne' ++ points se' ++ points sw' ++ points nw' 

-- get all points within distance r for every point in tree
pointsInDistance :: Float -> Quadtree -> [(Point, [Point])]
pointsInDistance r q = map (\x -> (,) x . filter (/=x) $ query (x P.- (r,r)) (x P.+ (r,r)) q) $ points q
