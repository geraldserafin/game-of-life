module Main where

import Graphics.Gloss (Point)
import Quadtree (Quadtree (Empty), getPointsNeighbours, insert)

data Cell = Alive | Dead deriving (Eq, Show)

aliveNeighbours :: Quadtree Cell -> [(Point, Cell, Int)]
aliveNeighbours = map (\(a,b,c) -> (a,b, length $ filter (==Alive) c)) . getPointsNeighbours

insertAlive :: (Float, Float) -> Quadtree Cell -> Quadtree Cell
insertAlive (x,y) t = foldl (\t' p -> insert p Dead t') (insert (x,y) Alive t) neighbourIndexes 
  where 
    neighbourIndexes = [(x',y') | x' <- [x-1..x+1], y' <- [y-1..y+1], (x',y') /= (x,y) ]

main :: IO ()
main = print . aliveNeighbours . insertAlive (0,0) $ Empty
