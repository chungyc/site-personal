module Main (main) where

import Diagrams.Runner
import Diagrams.Transform.Matrix (fromMat22)
import Physics.Spacetime.Flat (axesLength, axesOptions, axesWith)
import Physics.Spacetime.Flat qualified as Spacetime

main :: IO ()
main = putDiagram defaultOptions observedAxes

observedAxes :: Diagram B
observedAxes = otherFrame <> originalFrame <> phantomBox
  where
    otherFrame =
      axesWith axesOptions {axesLength = 2}
        # opacity 0.25
        # lineColor red
    -- As observed from the other frame moving at 0.5c,
    -- if the coordinate transformation was non-relativistic.
    originalFrame = diagram # transform transformation
    transformation = fromMat22 (V2 (V2 1 (-0.5)) (V2 0 1)) zero

-- | Spacetime diagram in the original frame with axes and word line.
diagram :: Diagram B
diagram = axes <> worldLine

axes :: Diagram B
axes = axesWith axesOptions {axesLength = 2}

-- | A world line starting from the origin with a velocity of 0.5c.
worldLine :: Diagram B
worldLine =
  strokeLine (fromOffsets [V2 1 2])
    # lineWidth thick
    # lineColor blue

-- | An invisible box so that the diagram sizes are comparable
-- to other diagrams on the page.
phantomBox :: Diagram B
phantomBox = phantom $ fromCorners ((-5) ^& (-2)) (5 ^& 2)
