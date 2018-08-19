module TwoDimShapes
    ( Line(..), TwoVec(..), vecSum, scalProd, dotProd, randVec, lineEq, aboveLine
    ) where

import System.Random

--Begin line/point definitions

--Define a 2-D vector
data TwoVec = TwoVec Double Double deriving (Show)

--Sum of two vectors
vecSum :: TwoVec -> TwoVec -> TwoVec
vecSum (TwoVec x1 y1) (TwoVec x2 y2) = TwoVec (x1+x2) (y1+y2)

--scalar product
scalProd :: TwoVec -> Double -> TwoVec
scalProd (TwoVec x y) s = TwoVec (s*x) (s*y)

--dot product
dotProd :: TwoVec -> TwoVec -> Double
dotProd (TwoVec x1 y1) (TwoVec x2 y2) = x1*x2 + y1*y2

--Generate a random two vec from a seed and a range.
randVec :: Int -> (Double,Double) -> TwoVec
randVec seed range = TwoVec x y where
    randomGen = mkStdGen seed
    (x, r) = randomR range randomGen
    (y, _) = randomR range r

data Line = Line {slope :: Double, offset :: Double} deriving (Show)

--Given an x value, calculate the line's y value
lineEq :: Line -> Double -> Double
lineEq l x = y where
    y = (slope l) * x + (offset l)

--Given a point and a line, calculate whether it lies above or below the line - 1 for above, 0 for below
aboveLine :: TwoVec -> Line -> Double
aboveLine (TwoVec x y) l = a where
    lineY = lineEq l x
    a = if y > lineY then 1.0 else 0.0