module Main where

import TwoDimShapes
import Perceptron

main :: IO ()
main = print test where
    test = out
    seed = 5
    iters = 100
    learningRate = 0.1
    initPercep = randPerceptron seed heaviside
    testLine = Line 1 0 --y=x, classic
    trainedP = train initPercep testLine iters learningRate seed
    vecBelow = TwoVec 0 1
    vecAbove = TwoVec 1 0
    out = (forwardPass trainedP vecBelow, forwardPass trainedP vecAbove)