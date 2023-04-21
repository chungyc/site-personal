module Main (main) where

import Diagrams.Runner
import Physics.Spacetime.Flat (axesLength, axesLightcone, axesOptions, axesWith)
import Physics.Spacetime.Flat qualified as Spacetime

main :: IO ()
main = putDiagram defaultOptions $ hcat [diagram, observedAxes] <> lengthLine
  where
    -- This measures the length of the time in the original frame.
    lengthLine =
      strokeLine (fromOffsets [V2 12 0])
        # translateX (-1)
        # translateY 2
        # dashingN [0.01, 0.02] 0
        # lineWidth thin

observedAxes :: Diagram B
observedAxes = otherFrame <> originalFrame
  where
    otherFrame =
      axesWith axesOptions {axesLength = 2}
        # opacity 0.25
        # lineColor red
    -- As observed from the other frame moving at 0.9c.
    originalFrame = diagram # transform (Spacetime.transformation $ -0.9)

-- | Spacetime diagram in the original frame with axes and word line.
diagram :: Diagram B
diagram = axes <> worldLine

axes :: Diagram B
axes = axesWith axesOptions {axesLength = 2, axesLightcone = False} # opacity 0.25

-- | A world line staying still for time 2.
worldLine :: Diagram B
worldLine =
  strokeLine (fromOffsets [V2 0 2])
    # lineWidth veryThick
    # lineColor blue
