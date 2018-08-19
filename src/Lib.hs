module Lib
    ( Perceptron(..), train, randPerceptron, Line(..), heaviside, TwoVec(..), forwardPass
    ) where

import System.Random
--Begin Perceptron defs

--Heaviside step function
heaviside :: Double -> Double
heaviside x = if x > 0 then 1 else 0

--Define a state of the perceptron - a two vector containing the weights, and a double with the bias
data PercepState = PercepState {weights :: TwoVec, bias :: Double} deriving (Show)

--Define a perceptron - a state plus an activation function
data Perceptron =  Perceptron {state :: PercepState, activation :: (Double -> Double)}

--Generate a random perceptron from a seed and an activation function
randPerceptron :: Int -> (Double -> Double) -> Perceptron
randPerceptron seed activation = Perceptron state activation where
    range = ((-100.0),100.0)
    w = randVec seed range
    randomGen = mkStdGen (seed+1) --Saves passing around a random generator, and what kind of deranged maniac would use intmax as a seed anyway?
    (b, _) = randomR range randomGen
    state = PercepState w b

{-
Define a forward pass
Take in a perceptron and a point, take the weighted sum and add bias, then apply activation function
-}
forwardPass :: Perceptron -> TwoVec -> Double
forwardPass p v = prediction where
    prediction = (activation p) biasSum
    biasSum = weightedSum + (bias $ state p)
    weightedSum = dotProd (weights $ state p) v

--Given a deviation from a correct answer for a given point, return a perceptron with adjusted weights and bias according to the learning rate
adjust :: Perceptron -> TwoVec -> Double -> Double -> Perceptron
adjust pIn p d lr = pOut where
    biasOut = (bias $ state pIn) + d*lr
    weightsOut = vecSum (weights $ state pIn) (scalProd p (d*lr)) 
    pOut = Perceptron (PercepState weightsOut biasOut) (activation pIn)

--Given a perceptron, a line, and a point, calculate the deviation of the perceptron's prediction from the actual value
deviation :: Perceptron -> Line -> TwoVec -> Double
deviation p l v = expected - actual where
    expected = forwardPass p v
    actual = aboveLine v l

--Given a perceptron, a line, a point, and a learning rate, learn from it
learn :: Perceptron -> Line -> TwoVec -> Double -> Perceptron
learn pIn l v lr = adjust pIn v d lr where
    d = deviation pIn l v

{-
Train a perceptron.
Inputs:
Perceptron, line, iters, learning rate, random seed
We need a random seed because pure functions are important.

Generate iters points randomly
Learn from each point
Return trained perceptron
-}
train :: Perceptron -> Line -> Int -> Double -> Int -> Perceptron
train pIn l iters lr seed = if iters > 0 then train pOut l (iters-1) lr (seed+1) else pIn where
    pOut = learn pIn l randP lr
    randP = randVec seed ((-range),range)
    range = abs (2 * (offset l))

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