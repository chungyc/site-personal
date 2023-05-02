module Main (main) where

import Diagrams.Backend.SVG
import Diagrams.Runner
import Physics.Spacetime.Flat (axesLength, axesOptions, axesWith)

main :: IO ()
main = putDiagram defaultOptions diagram

diagram :: Diagram B
diagram = axes <> worldLine <> phantomBox

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
