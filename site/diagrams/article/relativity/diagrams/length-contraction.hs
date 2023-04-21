module Main (main) where

import Diagrams.Runner
import Physics.Spacetime.Flat (axesLength, axesLightcone, axesOptions, axesWith)
import Physics.Spacetime.Flat qualified as Spacetime

main :: IO ()
main = putDiagram defaultOptions $ vcat [diagram, observedAxes] <> lengthLines
  where
    -- These measure the length of the line in the original frame.
    lengthLines =
      (lengthLineA <> lengthLineB)
        # translateY 2
        # dashingN [0.01, 0.02] 0
        # lineWidth thin
    lengthLineA =
      strokeLine (fromOffsets [V2 0 (-12)])
        # translateX (-1)
    lengthLineB =
      strokeLine (fromOffsets [V2 0 (-12)])
        # translateX 1

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

-- | Two world lines for both ends of a single stick of length 4.
-- The middle of the stick will pass through the origin.
worldLine :: Diagram B
worldLine =
  (endA <> endB)
    # lineWidth veryThick
    # lineColor blue
  where
    endA = strokeLine (fromOffsets [V2 0 2]) # translate (V2 (-1) (-0.5))
    endB = strokeLine (fromOffsets [V2 0 2]) # translate (V2 1 (-1.5))
