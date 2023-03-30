module Main where

import Quadtree (Quadtree (Empty), insert, nextGeneration, points)
import Control.Monad.Random (MonadRandom(getRandomRs))
import Graphics.Gloss (Display (InWindow), black, pictures, rectangleSolid, Picture (Color), white, translate, scale, simulate)
import GHC.Float (int2Float)

display :: Display
display = InWindow "Game of Life" (800,800) (100,100)

pointToFloat :: (Int, Int) -> (Float, Float)
pointToFloat (x,y) = (int2Float x, int2Float y)

generationAsPicture :: Quadtree -> Picture
generationAsPicture = scale 4 4 . pictures . map cellPicture . points
  where 
    cellPicture (x,y) = translate x y . Color white $ rectangleSolid 1 1 

main :: IO ()
main = do
  randomPoints <- getRandomRs ((-100,-100), (100,100))
  let initialWorld = foldl insert Empty (map pointToFloat $ take 5000 randomPoints)

  simulate display black 20 initialWorld generationAsPicture (\_ _ z -> nextGeneration z)
