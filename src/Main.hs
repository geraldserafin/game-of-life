module Main where

import Quadtree (leaf, insert, query, points, pointsInDistance)

main :: IO ()
main = print . pointsInDistance 1 . insert (-1,-1) . insert (1,1) . insert (2,2) $ insert (0,0) leaf
