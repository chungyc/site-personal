module Main (main) where

import Diagrams.Runner
import Physics.Spacetime.Flat qualified as Spacetime

main :: IO ()
main = putDiagram defaultOptions $ observedAxes

observedAxes :: Diagram B
observedAxes = otherFrame <> originalFrame
  where
    otherFrame = Spacetime.axes # opacity 0.25 # lc red
    originalFrame = Spacetime.axes # transform (Spacetime.transformation 0.5)
