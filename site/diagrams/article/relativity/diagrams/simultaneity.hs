module Main (main) where

import Diagrams.Runner
import Physics.Spacetime.Flat (axesLength, axesLightcone, axesOptions, axesWith)
import Physics.Spacetime.Flat qualified as Spacetime

main :: IO ()
main = putDiagram defaultOptions $ hcat [diagram, observedAxes]

observedAxes :: Diagram B
observedAxes = otherFrame <> originalFrame
  where
    otherFrame =
      axesWith axesOptions {axesLength = 2, axesLightcone = False}
        # opacity 0.25
        # lineColor red
    -- As observed from the other frame moving at 0.9c.
    originalFrame = diagram # transform (Spacetime.transformation $ -0.9)

-- | Spacetime diagram in the original frame with axes and word line.
diagram :: Diagram B
diagram = axes <> worldLine

axes :: Diagram B
axes = axesWith axesOptions {axesLength = 2, axesLightcone = False} # opacity 0.25

-- | Two world lines that are stationary in the original frame.
-- They have different colors above and below the \(t=0\) axies in the original frame.
worldLine :: Diagram B
worldLine = (upperLines <> lowerLines) # lineWidth veryThick
  where
    endA = strokeLine (fromOffsets [V2 0 2]) # translate (V2 (-1) 0)
    endB = strokeLine (fromOffsets [V2 0 2]) # translate (V2 1 0)
    upperLines = (endA <> endB) # lineColor blue # dashingL [0.1, 0.3] 0
    endC = strokeLine (fromOffsets [V2 0 (-2)]) # translate (V2 (-1) 0)
    endD = strokeLine (fromOffsets [V2 0 (-2)]) # translate (V2 1 0)
    lowerLines = (endC <> endD) # lineColor red # dashingL [0.3, 0.1] 0
