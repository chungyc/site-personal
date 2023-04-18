module Main (main) where

import Diagrams.Runner
import Physics.Spacetime.Flat qualified as Spacetime

main :: IO ()
main = putDiagram defaultOptions Spacetime.axes
