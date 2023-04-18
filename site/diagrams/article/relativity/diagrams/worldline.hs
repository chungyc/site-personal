module Main (main) where

import Diagrams.Runner
import Physics.Spacetime.Flat (axesLength, axesOptions, axesWith)

main :: IO ()
main = putDiagram defaultOptions diagram

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
